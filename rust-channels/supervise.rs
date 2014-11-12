#![feature(macro_rules)]

use std::any::{Any, AnyRefExt};
use std::comm::Select;
use std::fmt::{mod, Show};
use std::io::timer::Timer;
use std::sync::atomic::{AtomicUint, SeqCst, INIT_ATOMIC_UINT};
use std::time::Duration;

#[deriving(Show)]
enum ExitSignal {
    Exited,
    Panicked
}

#[deriving(Show)]
enum BasicServiceMsg {
    Panic,
    Stop
}

enum SupervisorMsg {
    //AddChild (Box<Service<ServiceControls + 'static + Send> + Send>),
    AddChild (Box<Any + Send>),
    // TODO: simplify to just Stop once split into modules is done
    StopSupervisor
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
trait Service<Controls: ServiceControls>: Send {

    // Called to start (using spawn) the service in a new task.
    fn start(handle: Handle) -> Controls;

}

// ServiceControls is an interface for interacting with the service,
// which, unlike its state, can be freely copied.
trait ServiceControls {

    // Called to stop execution, most probably from outside of the service's task.
    fn stop(&self);

}

struct BasicService;

struct BasicServiceState {
    tx: Sender<BasicServiceMsg>,
    rx: Receiver<BasicServiceMsg>
}

struct BasicServiceControls {
    tx: Sender<BasicServiceMsg>
}

impl BasicService {

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
                let msg = rx.recv();
                if !handle_message(msg)
                    { break  }
            } else if ret == timeout.id() {
                let () = timeout.recv();
                println!("timed out, no message received in {} seconds", seconds)
            }
        }
    }

}

impl Service<BasicServiceControls> for BasicService {

    fn start(handle: Handle) -> BasicServiceControls {
        let (tx, rx) = channel();
        let control_tx = tx.clone();
        let state = BasicServiceState { tx: tx, rx: rx };
        spawn(proc() BasicService.serve(handle, state));
        BasicServiceControls { tx: control_tx }
    }

}

impl ServiceControls for BasicServiceControls {

    fn stop(&self) {
        self.tx.send(Stop);
    }

}

fn handle_message(msg: BasicServiceMsg) -> bool {
    println!("received {}", msg);
    let proceed = match msg {
        Stop => false,
        Panic => panic!("intentional panic")
    };
    proceed
}

struct Supervisor;

struct SupervisorControls {
    id: SupervisorId,
    tx: Sender<SupervisorMsg>,
    exit_tx: Sender<ExitSignal>
}

struct SupervisorState {
    id: SupervisorId,
    current_service_id: uint,
    tx: Sender<SupervisorMsg>,
    rx: Receiver<SupervisorMsg>,
    exit_tx: Sender<ExitSignal>,
    exit_rx: Receiver<ExitSignal>

}

static CURRENT_SUPERVISOR_ID : AtomicUint = INIT_ATOMIC_UINT;

type SupervisorId = uint;

#[deriving(Show)]
struct ServiceId {
    sup: SupervisorId,
    service: uint
}

impl Supervisor {

    pub fn start() -> SupervisorControls {
        Service::start(Handle(None))
    }

    fn start_with_handle_id(handle: Handle, id: uint) -> SupervisorControls {
        let (tx, rx) = channel();
        let (exit_tx, exit_rx) = channel();
        let controls_tx = tx.clone();
        let state =
            SupervisorState { id: id, current_service_id: 0,
                              tx: tx, rx: rx,
                              exit_tx: exit_tx.clone(), exit_rx: exit_rx };
        spawn(proc() Supervisor.serve(handle, state));
        SupervisorControls { id: id, tx: controls_tx, exit_tx: exit_tx }
    }

    fn serve(&self, _: Handle, s: SupervisorState) {
        loop {
            match s.exit_rx.recv() {
                Exited | Panicked =>
                    println!("child died")
            };
            //println!("supervisor exiting");
            //break
        }
    }

}

impl Service<SupervisorControls> for Supervisor {

    fn start(handle: Handle) -> SupervisorControls {
        let id = CURRENT_SUPERVISOR_ID.fetch_add(1, SeqCst);
        Supervisor::start_with_handle_id(handle, id)
    }

}

impl SupervisorControls {

    fn add<C: ServiceControls, Child: Service<C>>
          (&self, service: Child) -> C {
        self.tx.send(AddChild(box service));
        let handle = Handle(Some(self.exit_tx.clone()));
        Service::start(handle)
    }

}

impl ServiceControls for SupervisorControls {

    fn stop(&self) { self.tx.send(StopSupervisor); }

}

impl Show for SupervisorControls {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "SupervisorControls {} id: {} {}", "{", self.id, "}")
    }
}

// Handle passed to the top-level supervisor does not have a supervisor channel.
struct Handle(Option<Sender<ExitSignal>>);

impl Drop for Handle {
    fn drop(&mut self) {
        match *self {
            Handle(None) => (),
            Handle(Some (ref tx)) => {
                if std::task::failing()
                    { tx.send(Panicked); }
                else
                    { tx.send(Exited); }
            }
        }
    }
}

fn main() {
    let sup = Supervisor::start();
    println!("sup = {}", sup);
    let controls = sup.add(BasicService);
    let mut timer = Timer::new().unwrap();
    let mut timeout = timer.oneshot(Duration::seconds(1));
    timeout.recv();
    //controls.ping();
    timeout = timer.oneshot(Duration::seconds(1));
    timeout.recv();
    controls.stop();
    //sup.remove(id);
}
