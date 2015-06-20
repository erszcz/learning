#![feature(macro_rules)]
#![feature(phase)]
#![no_std]
#![feature(globs)]
#[phase(plugin, link)]
extern crate "std" as std;
extern crate "native" as rt;
#[prelude_import]
use std::prelude::*;
use std::fmt::{mod, Show};
use std::io::timer::Timer;
use std::sync::atomic::{AtomicUint, SeqCst, INIT_ATOMIC_UINT};
use std::time::Duration;

enum SupMsg { Exited(ServiceId), Panicked(ServiceId), Panic, Stop, }
#[automatically_derived]
impl ::std::fmt::Show for SupMsg {
    fn fmt(&self, __arg_0: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match (&*self,) {
            (&Exited(ref __self_0),) =>
            match (&(*__self_0),) {
                (__arg0,) => {
                    #[inline]
                    #[allow(dead_code)]
                    static __STATIC_FMTSTR: [&'static str, ..2u] =
                        ["Exited(", ")"];
                    let __args_vec =
                        &[::std::fmt::argument(::std::fmt::Show::fmt,
                                               __arg0)];
                    let __args =
                        unsafe {
                            ::std::fmt::Arguments::new(__STATIC_FMTSTR,
                                                       __args_vec)
                        };
                    __arg_0.write_fmt(&__args)
                }
            },
            (&Panicked(ref __self_0),) =>
            match (&(*__self_0),) {
                (__arg0,) => {
                    #[inline]
                    #[allow(dead_code)]
                    static __STATIC_FMTSTR: [&'static str, ..2u] =
                        ["Panicked(", ")"];
                    let __args_vec =
                        &[::std::fmt::argument(::std::fmt::Show::fmt,
                                               __arg0)];
                    let __args =
                        unsafe {
                            ::std::fmt::Arguments::new(__STATIC_FMTSTR,
                                                       __args_vec)
                        };
                    __arg_0.write_fmt(&__args)
                }
            },
            (&Panic,) =>
            match () {
                () => {
                    #[inline]
                    #[allow(dead_code)]
                    static __STATIC_FMTSTR: [&'static str, ..1u] = ["Panic"];
                    let __args_vec = &[];
                    let __args =
                        unsafe {
                            ::std::fmt::Arguments::new(__STATIC_FMTSTR,
                                                       __args_vec)
                        };
                    __arg_0.write_fmt(&__args)
                }
            },
            (&Stop,) =>
            match () {
                () => {
                    #[inline]
                    #[allow(dead_code)]
                    static __STATIC_FMTSTR: [&'static str, ..1u] = ["Stop"];
                    let __args_vec = &[];
                    let __args =
                        unsafe {
                            ::std::fmt::Arguments::new(__STATIC_FMTSTR,
                                                       __args_vec)
                        };
                    __arg_0.write_fmt(&__args)
                }
            },
        }
    }
}

trait Service: Send {

    // Called by the supervisor in a new task.
    fn serve(self, h: Handle);

    // Called to stop execution, most probably from outside of the service's task.
    fn stop(self);

}

struct BasicService {
    tx: Sender<SupMsg>,
    rx: Receiver<SupMsg>,
}

impl Service for BasicService {

    fn serve(self, _: Handle) {
        let mut timer = Timer::new().unwrap();
        let rx = self.rx;
        loop  {
            let seconds = 1;
            let timeout = timer.oneshot(Duration::seconds(seconds));














            //println!("supervisor exiting");
            //break





            //s_tx.send(Panic);
            {
                use std::comm::Select;
                let sel = Select::new();
                let mut rx = sel.handle(&rx);
                let mut timeout = sel.handle(&timeout);
                unsafe { rx.add(); timeout.add(); }
                let ret = sel.wait();
                if ret == rx.id() {
                    let sup_msg = rx.recv();
                    if !handle_message(sup_msg) { break  }
                } else if ret == timeout.id() {
                    let () = timeout.recv();
                    match (&seconds,) {
                        (__arg0,) => {
                            #[inline]
                            #[allow(dead_code)]
                            static __STATIC_FMTSTR: [&'static str, ..2u] =
                                ["timed out, no message received in ",
                                 " seconds"];
                            let __args_vec =
                                &[::std::fmt::argument(::std::fmt::Show::fmt,
                                                       __arg0)];
                            let __args =
                                unsafe {
                                    ::std::fmt::Arguments::new(__STATIC_FMTSTR,
                                                               __args_vec)
                                };
                            ::std::io::stdio::println_args(&__args)
                        }
                    }
                } else {
                    {
                        static _FILE_LINE: (&'static str, uint) =
                            ("supervise.rs", 39u);
                        ::std::rt::begin_unwind("internal error: entered unreachable code",
                                                &_FILE_LINE)
                    }
                }
            }
        }
    }
    fn stop(self) { self.tx.send(Stop); }
}
fn handle_message(msg: SupMsg) -> bool {
    match (&msg,) {
        (__arg0,) => {
            #[inline]
            #[allow(dead_code)]
            static __STATIC_FMTSTR: [&'static str, ..1u] = ["received "];
            let __args_vec =
                &[::std::fmt::argument(::std::fmt::Show::fmt, __arg0)];
            let __args =
                unsafe {
                    ::std::fmt::Arguments::new(__STATIC_FMTSTR, __args_vec)
                };
            ::std::io::stdio::println_args(&__args)
        }
    };
    let proceed =
        match msg {
            Stop => false,
            Panic => {
                static _FILE_LINE: (&'static str, uint) =
                    ("supervise.rs", 59u);
                ::std::rt::begin_unwind("intentional panic", &_FILE_LINE)
            }
            _ => true,
        };
    proceed
}
impl Send for BasicService { }
struct Supervisor {
    id: SupervisorId,
    current_service_id: AtomicUint,
    tx: Sender<SupMsg>,
    rx: Receiver<SupMsg>,
    services: Vec<Box<Service>>,
}
impl Show for Supervisor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        {
            match (&self.id,) {
                (__arg0,) => {
                    #[inline]
                    #[allow(dead_code)]
                    static __STATIC_FMTSTR: [&'static str, ..2u] =
                        ["Supervisor[id: ", "]"];
                    let __args_vec =
                        &[::std::fmt::argument(::std::fmt::Show::fmt,
                                               __arg0)];
                    let __args =
                        unsafe {
                            ::std::fmt::Arguments::new(__STATIC_FMTSTR,
                                                       __args_vec)
                        };
                    f.write_fmt(&__args)
                }
            }
        }
    }
}
static CURRENT_SUPERVISOR_ID: AtomicUint = INIT_ATOMIC_UINT;
type SupervisorId = uint;
struct ServiceId {
    sup: SupervisorId,
    service: uint,
}
#[automatically_derived]
impl ::std::fmt::Show for ServiceId {
    fn fmt(&self, __arg_0: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            ServiceId { sup: ref __self_0_0, service: ref __self_0_1 } =>
            match (&(*__self_0_0), &(*__self_0_1)) {
                (__arg0, __arg1) => {
                    #[inline]
                    #[allow(dead_code)]
                    static __STATIC_FMTSTR: [&'static str, ..3u] =
                        ["ServiceId { sup: ", ", service: ", " }"];
                    let __args_vec =
                        &[::std::fmt::argument(::std::fmt::Show::fmt, __arg0),
                          ::std::fmt::argument(::std::fmt::Show::fmt,
                                               __arg1)];
                    let __args =
                        unsafe {
                            ::std::fmt::Arguments::new(__STATIC_FMTSTR,
                                                       __args_vec)
                        };
                    __arg_0.write_fmt(&__args)
                }
            },
        }
    }
}
impl Supervisor {
    fn new() -> Supervisor {
        let (tx, rx) = channel();
        Supervisor{id: CURRENT_SUPERVISOR_ID.fetch_add(1, SeqCst),
                   current_service_id: INIT_ATOMIC_UINT,
                   tx: tx,
                   rx: rx,
                   services:
                       {
                           use std::slice::BoxedSlicePrelude;
                           let xs: ::std::boxed::Box<[_]> = box() [];
                           xs.into_vec()
                       },}
    }
    fn add<Child: Service>(&self, service: Child) {
        let child_tx = self.tx.clone();
        let service_id =
            ServiceId{sup: self.id,
                      service: self.current_service_id.fetch_add(1, SeqCst),};
        spawn(proc() { let child_tx = child_tx;
              let handle =
                  Handle{tx: child_tx.clone(), service_id: service_id,};
              service.serve(handle); });
        self.services.push(box() service as Box<Service>);
    }
    fn supervise(&self) {
        loop  {
            match self.rx.recv() {
                Exited(service) | Panicked(service) =>
                match (&service,) {
                    (__arg0,) => {
                        #[inline]
                        #[allow(dead_code)]
                        static __STATIC_FMTSTR: [&'static str, ..2u] =
                            ["child ", " died"];
                        let __args_vec =
                            &[::std::fmt::argument(::std::fmt::Show::fmt,
                                                   __arg0)];
                        let __args =
                            unsafe {
                                ::std::fmt::Arguments::new(__STATIC_FMTSTR,
                                                           __args_vec)
                            };
                        ::std::io::stdio::println_args(&__args)
                    }
                },
                other =>
                match (&other,) {
                    (__arg0,) => {
                        #[inline]
                        #[allow(dead_code)]
                        static __STATIC_FMTSTR: [&'static str, ..1u] =
                            ["sup received "];
                        let __args_vec =
                            &[::std::fmt::argument(::std::fmt::Show::fmt,
                                                   __arg0)];
                        let __args =
                            unsafe {
                                ::std::fmt::Arguments::new(__STATIC_FMTSTR,
                                                           __args_vec)
                            };
                        ::std::io::stdio::println_args(&__args)
                    }
                },
            };
        }
    }
}
struct Handle {
    tx: Sender<SupMsg>,
    service_id: ServiceId,
}
impl Drop for Handle {
    fn drop(&mut self) {
        if std::task::failing() {
            self.tx.send(Panicked(self.service_id));
        } else { self.tx.send(Exited(self.service_id)); }
    }
}
fn main() {
    let (tx, rx) = channel();
    let s_tx = tx.clone();
    let service = BasicService{tx: tx, rx: rx,};
    let sup = Supervisor::new();
    let sup2 = Supervisor::new();
    match (&sup, &sup2) {
        (__arg0, __arg1) => {
            #[inline]
            #[allow(dead_code)]
            static __STATIC_FMTSTR: [&'static str, ..2u] = ["", " "];
            let __args_vec =
                &[::std::fmt::argument(::std::fmt::Show::fmt, __arg0),
                  ::std::fmt::argument(::std::fmt::Show::fmt, __arg1)];
            let __args =
                unsafe {
                    ::std::fmt::Arguments::new(__STATIC_FMTSTR, __args_vec)
                };
            ::std::io::stdio::println_args(&__args)
        }
    };
    sup.add(service);
    spawn(proc() sup.supervise());
    let mut timer = Timer::new().unwrap();
    let timeout = timer.oneshot(Duration::seconds(2));
    timeout.recv();
    s_tx.send(Stop);
}
