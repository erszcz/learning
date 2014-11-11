#![feature(macro_rules)]

use std::comm::Select;
use std::fmt::{mod, Show};
use std::io::timer::Timer;
use std::sync::atomic::{AtomicUint, SeqCst, INIT_ATOMIC_UINT};
use std::time::Duration;

#[deriving(Show)]
enum SupMsg {
    Exited (ServiceId),
    Panicked (ServiceId),
    Panic,
    Stop
}

// Service is a service description.
//
// Due to unique ownership of a Receiver end of a channel a service
// can't store a receiver in its struct implementing the Service trait.
// Therefore, the channel(s) has(ve) to be created in Service::start(),
// Controls can store the Sender(s) and ServiceState the Receiver(s).
// The Controls should returned to the outside world and can be freely copied,
// while the ServiceState should be moved to Service::serve method
// started in its own task.
trait Service<Controls: ServiceControls, ServiceState>: Send {

    // Called to start (using spawn) the service in a new task.
    fn start(handle: Handle) -> Controls;

    // Called by the supervisor in a new task.
    fn serve(&self, h: Handle, s: ServiceState);

}

// ServiceControls is an interface for interacting with the service,
// which, unlike its state, can be freely copied.
trait ServiceControls: Send {

    // Called to stop execution, most probably from outside of the service's task.
    fn stop(&self);

}

struct BasicService;

struct BasicServiceState {
    tx: Sender<SupMsg>,
    rx: Receiver<SupMsg>
}

struct BasicServiceControls {
    tx: Sender<SupMsg>
}

impl Service<BasicServiceControls, BasicServiceState> for BasicService {

    fn start(handle: Handle) -> BasicServiceControls {
        let (tx, rx) = channel();
        let control_tx = tx.clone();
        let state = BasicServiceState { tx: tx, rx: rx };
        spawn(proc() BasicService.serve(handle, state));
        BasicServiceControls { tx: control_tx }
    }

    fn serve(&self, _: Handle, s: BasicServiceState) {
        let mut timer = Timer::new().unwrap();
        loop {
            let seconds = 1;
            let timeout = timer.oneshot(Duration::seconds(seconds));
            let sel = Select::new();
            let mut rx = sel.handle(&s.rx);
            let mut timeout = sel.handle(&timeout);
            unsafe {
                rx.add();
                timeout.add();
            }
            let ret = sel.wait();
            if ret == rx.id() {
                let sup_msg = rx.recv();
                if !handle_message(sup_msg)
                    { break  }
            } else if ret == timeout.id() {
                let () = timeout.recv();
                println!("timed out, no message received in {} seconds", seconds)
            }
        }
    }

}

impl ServiceControls for BasicServiceControls {

    fn stop(&self) {
        self.tx.send(Stop);
    }

}

fn handle_message(msg: SupMsg) -> bool {
    println!("received {}", msg);
    let proceed = match msg {
        Stop => false,
        Panic => panic!("intentional panic"),
        _ => true
    };
    proceed
}

struct Supervisor {
    id: SupervisorId,
    current_service_id: AtomicUint,
    tx: Sender<SupMsg>,
    rx: Receiver<SupMsg>
}

impl Show for Supervisor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Supervisor {} id: {} {}", "{", self.id, "}")
    }
}

static CURRENT_SUPERVISOR_ID : AtomicUint = INIT_ATOMIC_UINT;

type SupervisorId = uint;

#[deriving(Show)]
struct ServiceId {
    sup: SupervisorId,
    service: uint
}

impl Supervisor {

    fn new() -> Supervisor {
        let (tx, rx) = channel();
        Supervisor { id: CURRENT_SUPERVISOR_ID.fetch_add(1, SeqCst),
                     current_service_id: INIT_ATOMIC_UINT,
                     tx: tx, rx: rx }
    }

    fn add<C: ServiceControls,
           ServiceState,
           Child: Service<C, ServiceState>>(&self, service: Child) -> C {
        let child_tx = self.tx.clone();
        let service_id =
            ServiceId { sup: self.id,
                        service: self.current_service_id.fetch_add(1, SeqCst) };
        let handle = Handle { tx: child_tx.clone(),
                              service_id: service_id };
        Service::start(handle)
    }

    fn supervise(&self) {
        loop {
            match self.rx.recv() {
                Exited (service) | Panicked (service) =>
                    println!("child {} died", service),
                other =>
                    println!("sup received {}", other)
            };
            //println!("supervisor exiting");
            //break
        }
    }

}

struct Handle {
    tx: Sender<SupMsg>,
    service_id: ServiceId
}

impl Drop for Handle {
    fn drop(&mut self) {
        if std::task::failing()
            { self.tx.send(Panicked (self.service_id)); }
        else
            { self.tx.send(Exited (self.service_id)); }
    }
}

fn main() {
    let service = BasicService;
    let sup = Supervisor::new();
    let sup2 = Supervisor::new();
    println!("{}\n{}", sup, sup2);
    let controls = sup.add(service);
    spawn(proc() sup.supervise());

    let mut timer = Timer::new().unwrap();
    let timeout = timer.oneshot(Duration::seconds(2));
    timeout.recv();
    //controls.tx.send(Stop);
    controls.tx.send(Panic);
}
