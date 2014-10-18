type TaskId = uint;

enum Msg {
    Sync (TaskId),
    Result (Vec<uint>)
}

impl Msg {

    fn get_sync(&self) -> TaskId {
        match *self {
            Sync (id) => id,
            _ => fail!()
        }
    }

    fn get_result(&self) -> &Vec<uint> {
        match *self {
            Result (ref vec) => vec,
            _ => fail!()
        }
    }

}

fn main() {
    let id = 1u;
    let ntasks = 3;
    let (tx, rx) = channel();

    // In order to keep access to `v` a copy must be made explicitly.
    // Otherwise, all accesses after `spawn` will be invalid.
    //let v = vec!(id);
    //spawn(proc() { mytask(id+1, ntasks, v, tx) });
    //println!("[{}] got sync from {}", id, rx.recv().get_sync());
    // Invalid access to `v`: error: use of moved value: `v`
    //println!("myv after first sync: {}", v);

    let v = vec!(id);
    //spawn(proc() { mytask(id+1, ntasks, v, tx) });
    spawn(proc() { mytask(id+1, ntasks, v, tx) });
    println!("[{}] got sync from {}", id, rx.recv().get_sync());

    println!("[{}] got sync from {}", id, rx.recv().get_sync());
    let maybe_v2 = rx.recv();
    let v2 = maybe_v2.get_result();
    println!("v2: {}", v2);
}

fn mytask(id: TaskId, ntasks: uint, mut v: Vec<uint>, roottx: Sender<Msg>) {
    v.push(id);
    roottx.send(Sync (id));
    if id == ntasks {
        roottx.send(Result (v));
        return
    }
    spawn(proc() { mytask(id+1, ntasks, v, roottx) });
}
