High priority:

[ ] Support alpha for glyph (big change)
[ ] Support fullscreen, plus different strategies when it comes to this.
[ ] Add event helper for focus ring
[ ] Debug issue with clicks
[ ] Debug issue with smoothness
[ ] Add customisable default handlers for components to override keys. This has become even more urgent: see ButtonDemo.hs, it makes no sense to have to use up / down arrow keys here.

Medium priority:

[ ] Particle utility, whatever form it might have...
[ ] Generate haddock documentation and see what needs documentation
[ ] Add a basic modal helper
[ ] Some tooltip / hovering helper ?

Low priority:

[ ] Add multiline text component
[ ] Ensure LogConsole doesn't display excessive content (beyond visible height)
[ ] Add a more comprehensive test suite
[ ] Add FPS overlay
[ ] Have some benchmarking suite to be able to experiment with performance improvement (grouping draw calls with similar colour, e.g. - or even simply avoiding some SDL calls that are redundant)
[ ] Add an intermediary demo illustrating component composition
