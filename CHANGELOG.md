# Revision history for atomic-css

## atomic-css 0.1.0

This package renamed to atomic-css with a focus on css utilities. View, Url and other Hyperbole-specific types moved to Hyperbole. Still provides an Html monad
Major rewrite of Library and API
  * New interface with operators: (@) for attributes, (~) to utilities
  * Defining custom CSS and new utilities is more intuitive

## web-view 0.7.0

* stack, popup, offset, layer - more intuitive interface
* added Web.View.Url.renderPath
* Style class
* added code, lists

## web-view 0.6.0

* stack - layout children on top of each other
* ChildCombinator: apply styles to direct children
* `Mod` is now `Mod context`, allowing for type-safe `Mod`s
* fixed: escaping in auto-generated `<style>`
* Refactored: selectors and rendering

## web-view 0.5.0

* Rendering improvements
* extClass to add external css class
* inline elements
* Url: no longer lowercases automatically. Show/Read instance
* 

## web-view 0.4.0

* Added new Mods. Length type. Improved Url type

## web-view 0.3.1

* Cleanup. Refactored Mods

## web-view 0.2.3 -- 2023-11-27

* First version. Released on an unsuspecting world.
