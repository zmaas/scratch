package main

import (
	"bufio"
	"fmt"
	"github.com/jroimartin/gocui"
	"github.com/logrusorgru/aurora"
	"log"
	"os"
	"os/exec"
	"os/user"
	"strings"
	"time"
)

var au aurora.Aurora

func init() {
	au = aurora.NewAurora(true)
}

func main() {
	// Generate UI
	g, err := gocui.NewGui(gocui.OutputNormal)
	if err != nil {
		log.Panicln(err)
	}
	defer g.Close()

	g.SetManagerFunc(layout)

	// Keybindings
	if err := g.SetKeybinding("", gocui.KeyCtrlC, gocui.ModNone, quit); err != nil {
		log.Panicln(err)
	}
	if err := g.SetKeybinding("", 'q', gocui.ModNone, quit); err != nil {
		log.Panicln(err)
	}
	if err := g.SetKeybinding("", 'r', gocui.ModNone, overwrite); err != nil {
		log.Panicln(err)
	}

	// Main loop
	if err := g.MainLoop(); err != nil && err != gocui.ErrQuit {
		log.Panicln(err)
	}
}

func overwrite(g *gocui.Gui, v *gocui.View) error {
	v.Overwrite = !v.Overwrite
	return nil
}

func layout(g *gocui.Gui) error {
	maxX, maxY := g.Size()
	// Program Info
	if v, err := g.SetView("user", 1, 0, maxX/2-1, 3); err != nil {
		if err != gocui.ErrUnknownView {
			return err
		}
		v.Title = "Info"
		me := getUsr()
		host := getHost()
		fmt.Fprintln(v, "User:", me, "\nHost:", host)
	}
	// User Status
	if v, err := g.SetView("prog", maxX/2+1, 0, maxX-1, 3); err != nil {
		if err != gocui.ErrUnknownView {
			return err
		}
		v.Title = "About"
		fmt.Fprintln(v, "slurmtop, v0.0.1\na small utility for monitoring slurm queues")
	}
	// Window containing current job status
	if v, err := g.SetView("jobs", 1, 4, maxX-1, maxY-4); err != nil {
		if err != gocui.ErrUnknownView {
			return err
		}
		v.Title = "Current Active Jobs"
		fmt.Fprintln(v, getSqueueStatus())
	}
	// Keybinding	window
	if v, err := g.SetView("keys", 1, maxY-3, maxX-1, maxY-1); err != nil {
		if err != gocui.ErrUnknownView {
			return err
		}
		// v.Title = "Test"
		fmt.Fprintln(v, "Keybindings: [q]uit, [r]efresh; last updated:", time.Now())
	}
	return nil
}

// Bail out
func quit(g *gocui.Gui, v *gocui.View) error {
	return gocui.ErrQuit
}

// For info pane
func getUsr() string {
	user, err := user.Current()
	if err != nil {
		log.Fatal(err)
	}
	return user.Username
}

// For info pane
func getHost() string {
	node, err := os.Hostname()
	if err != nil {
		log.Fatal(err)
	}
	return node
}

func getSqueueStatus() string {
	var cmdOut []byte
	var err error
	// args := []string{"-hu", getUsr()}
	args := []string{"-h"}
	cmd := "squeue"
	if cmdOut, err = exec.Command(cmd, args...).Output(); err != nil {
		// log.Fatal("There was an error running squeue command: ", err)
		// Just print out some junk
		return "Error running squeue command..."
	}
	out := string(cmdOut)
	lines := strings.Count(out, "\n") + 1
	scanner := bufio.NewScanner(strings.NewReader(out))
	trimmed := make([]string, lines)
	for scanner.Scan() {
		trimmed = append(trimmed, strings.TrimSpace(scanner.Text()))
	}
	concat := strings.TrimSpace(strings.Join(trimmed, "\n"))
	return concat
}
