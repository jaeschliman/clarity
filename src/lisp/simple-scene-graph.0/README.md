version 0 of a node-based scene graph
somewhat inspired from work done on https://github.com/jaeschliman/xmas
which was able to support 30,000+ animated nodes in CCL on a macbook pro 
with a good framerate in 2018. 

this library does not come close to that level of performance yet, but
the constraints are quite different and not much time has been spent in
optimization as of yet.

the current API of this module is clunky, and I already have ideas for a better representation
that more closely matches the intent of this module.

this module also includes some basic layout support, but it has not
had much time spent on it yet, and will change.

the general idea here is a 2.5d node-based scene graph, where each node maintains
a transform into and out of its local space.

there is also a notion of a 'world' and 'world-view', where 'world' is the root
node of an heirarchy, and a world-view represents a given display's point of view
in the world.

the world object is also responsible for accepting input events and dispatching
them to the appropriate node. generally, raw events are not dispatched, with the
exception of keyboard input. instead input is modeled as gestures, a given gesture
is dispatched once only at initialization, and afterwards the world node updates
the gesture in place, and invokes a method on the gesture and target so it may be handled.

some dirty-tracking is currently implemented, specifically children of the world node
are tracked for movement, overlap etc, and redrawn appropriately. not yet implemented
is per-node dirty region tracking, although that is coming, as it is necessary for
efficiency

also partially implemented is clipping/culling (only drawing what is currently visible)
currently the world node maintains a simple spatial index of all child nodes, and performs
a query a draw time to select those nodes which are visible (or close-enough to visible) and
only draws this subset. while working well enough for some purposes, the implementation is
not final, in particular the drawing query will currently include the viewports of all displays,
not just the active one -- this just hasn't been fixed yet.

pleasantly, clipping and culling allow for a large world with many nodes.
in a simple test, I was able to instantiate 20,000 plus fully functional text
editors, and browse among them without noticeable lag on my recent macbook air.

clipping/culling/windowing is a central idea in the current thinking for this
module. the general idea being that nothing should execute if no one is watching.

all that said, there is plenty of clean up left to do in this module, not
just in the design of the API, but also around modeling of various components in general
 for ex. it should support associating an input/output device with a 'user' so that 
 a user may query for the world views of all connected displays belonging to them and arrange
 them etc.
 
