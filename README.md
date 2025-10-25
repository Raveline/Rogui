# Rogui

This is a library to help managing the graphical user interfaces for Roguelikes
in Haskell, using SDL. It is inspired partly by Brick and by libtcod.

It features:

- Handling SDL initialisation, loading tilesets / charsets;
- A set of primitive graphics rendering and a DSL to manipulate these;
- A set of basic components;
- Basic management of mouse clicks.

Basically, the goal is to free users from the hassle of writing a main game loop
and part of the annoyance of having to write the UI.

What it is not:

- A complete roguelike library: there is no utility for terrain generation, FOV
computation, etc;
- A complete UI library: components and features are limited;
- An entity management system;

# Main concepts

## Brush

One of the main goal of Rogui is to support the possibility of having several
different tilesets, for roguelikes that come with a "tiles" view (and not a pure
ASCII one). This is done throough the notion of Brush: a texture containing tiles.

This comes at a price, however: dividing a pixel-based window into console cells
depends on the tiles size. If you use brushes with different tile sizes, some of
the utilities and default computed values might be off.

Brushes are mainly accessed through a enumerated type you'll have to define,
typically called `rb` in the library (for "References Brush").

## Console

SDL2 doesn't offer an easy way to create virtual consoles and blit them on each
others. The Console datatype offers an emulation of this. Each consoles maintain
their own local coordinates. Consoles are mostly used:

- To define "main areas" of your UI, where you will positions different part of
your game and interface;
- To support positioning when defining components.

Creating layout components require fiddling a bit with consoles, but if the
existing layouts are enough for you, you should just need them for the first
use-case.

Predefined consoles are mainly accessed through a enumerated type you'll have to
define, typically called `rc` in the library (for "References Console").

## Components

Components are defined with the following philosophy:

- A component is mostly a stateless rendering function;
- Application is responsible for maintaining the state;
- Components should have local coordinates;
- Layout components are responsible for positioning and sizing;
- Interactive components should offer a sensible default implementation for
their state and event handling.

Since writing a non-layout component is pretty straight-foward, the proposed
components are more suggestions of implementations than official,
"can-do-it-all" widgets.

Layout support is not as advanced as Brick's wonderful system, though it is inspired by it. You basically get:

- Greedy components that take all possible space; "natural" space taken by
components is not pre-computed, they'll get an equally divided space between all
greedy components.
- Fixed size components take the given, predetermined amount of space.

Main layout components coming with the library are vbox and hbox. A more
advanced grid component is also provided, but it typically requires more
ceremony.

Components are parametric over names, mostly for focus and event handling. Only
interactive components really need to be named. Like `rb` and `rc`, this is an
enumerated type you'll have to define, and which is typically called `n` in the
library.

## Events

Events are processed in order:

- During the main loop, SDL events are polled;
- The handling of inputs can generate customised events, some internal to Rogui
(e.g., FocusNext, FocusPrev), some specific to your applications (which is why
the Rogui main type is always parametric over `e`, for the custom event type of
your game).
- This means events can generate events (through `fireEvent`). A hardcoded
limit of 30 recursions will prevent an infinite loop, but you're responsible
for avoiding this to happen (debugging logs are planned but not yet implemented).
- You are also expected to implement a lot of "plumbing" yourself, notably
reacting to FocusNext and FocusPrev, since it is inherently dependent upon the
state of your application.

# Not implemented / potential future things

- The SDL backend used is the default SDL 2D accelerated API. No openGL /
vulkan, which frankly seemed overkill, but some people love writing shaders, so
it might be a good idea to add this backend, one day.

# Credits

- A lot of idea and the FocusRing code were borrowed from the splendid [Brick](https://hackage.haskell.org/package/brick) library.
- All colour constants were taken from [libtcod](https://github.com/libtcod/libtcod).
- The tileset used to test the TileGrid component is from [Shade](https://merchant-shade.itch.io/16x16-puny-dungeon).
- The tileset used for rogueharvest is from [VectoRaith](https://vectoraith.itch.io/)
