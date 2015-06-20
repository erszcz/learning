package main

import "fmt"

type Doctor interface {
    Name() string
}

type Pediatrician interface {
    Doctor
}

type Person interface {
    Visit(Doctor)
}

type Child interface {
    Visit(Pediatrician)
}

type MadDoctor struct {
    name string
}

func (md *MadDoctor) Name() string {
    return md.name
}

type MadPediatrician struct {
    name string
}

func (md *MadPediatrician) Name() string {
    return md.name
}

type John struct {}

func (j *John) Visit(d Doctor) {
    fmt.Println("John visits " + d.Name())
}

type Johnny struct {}

func (j *Johnny) Visit(p Pediatrician) {
    fmt.Println("Johnny visits pediatrician " + p.Name())
}

func main() {
    j := new(John)
    d := new(MadDoctor)
    d.name = "Dr Mad"

    j1 := new(Johnny)
    p1 := new(MadPediatrician)
    p1.name = "Dr Maddy"

    j.Visit(d)
    j.Visit(p1)
    j1.Visit(d)
    j1.Visit(p1)
}
