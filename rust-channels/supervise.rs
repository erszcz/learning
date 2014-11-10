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

trait Service: Send {

    // Called by the supervisor in a new task.
    fn serve(&self, h: Handle);

    // Called to stop execution, most probably from outside of the service's task.
    fn stop(&self);

}

struct BasicService {
    tx: Sender<SupMsg>,
    rx: Receiver<SupMsg>
}

impl Service for BasicService {

    fn serve(&self, _: Handle) {
        let mut timer = Timer::new().unwrap();
        loop {
            let seconds = 1;
            let timeout = timer.oneshot(Duration::seconds(seconds));
            let sel = Select::new();
            let mut rx = sel.handle(&self.rx);
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

    fn add<Child: Service>(&self, service: Child) {
        let child_tx = self.tx.clone();
        let service_id =
            ServiceId { sup: self.id,
                        service: self.current_service_id.fetch_add(1, SeqCst) };
        spawn(proc() {
            let child_tx = child_tx;
            let handle = Handle { tx: child_tx.clone(),
                                  service_id: service_id };
            service.serve(handle);
        });
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
    let (tx, rx) = channel();
    let s_tx = tx.clone();
    let service = BasicService { tx: tx, rx: rx };
    let sup = Supervisor::new();
    let sup2 = Supervisor::new();
    println!("{}\n{}", sup, sup2);
    sup.add(service);
    spawn(proc() sup.supervise());

    let mut timer = Timer::new().unwrap();
    let timeout = timer.oneshot(Duration::seconds(2));
    timeout.recv();
    //s_tx.send(Stop);
    s_tx.send(Panic);
}
