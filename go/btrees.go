package main

import "fmt"
import "tour/tree"

// Walk walks the tree t sending all values
// from the tree to the channel ch.
func Walk(t *tree.Tree, ch chan int) {
	type WalkF func(WalkF, *tree.Tree, chan int)
	F := func(f WalkF, t *tree.Tree, ch chan int) {
		if t.Left != nil {
			f(f, t.Left, ch)
		}
		ch <- t.Value
		if t.Right != nil {
			f(f, t.Right, ch)
		}
	}
	F(F, t, ch)
	close(ch)
}

// Same determines whether the trees
// t1 and t2 contain the same values.
func Same(t1, t2 *tree.Tree) bool {
	ch1 := make(chan int)
	ch2 := make(chan int)
	go Walk(t1, ch1)
	go Walk(t2, ch2)
	for {
		v1, ok1 := <-ch1
		v2, ok2 := <-ch2
		switch {
		case v1 != v2:
			return false
		case ok1 && !ok2:
			return false
		case !ok1 && ok2:
			return false
		case !ok1 && !ok2:
			break
		}
	}
	return true
}

func main() {
	/*
	ch := make(chan int)
	go Walk(tree.New(1), ch)
	for v := range ch {
		fmt.Println(v)
	}
	*/
	if Same(tree.New(1), tree.New(2)) {
		fmt.Println("same")
	} else {
		fmt.Println("different")
	}
	if Same(tree.New(1), tree.New(1)) {
		fmt.Println("same")
	} else {
		fmt.Println("different")
	}
}
