{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Frontend.Page.Resources (resources) where

import Obelisk.Frontend.GoogleAnalytics
import Data.Foldable (for_)
import Data.Text (Text)
import Reflex.Dom
import Frontend.CommonWidgets

resources :: (Analytics t m, DomBuilder t m) => Section m
resources = Section
  { _section_title = "Resources"
  , _section_content = do
    elClass "p" "description" $ text "This page contains educational and informative resources on the Reflex FRP ecosystem, organised by topic and format."
  , _section_subsections = [guides, workshops, presentations, discussionForums, downloads, frpExtras]
  }

data LinkItem m = LinkItem
  { _linkItem_title :: Text
  , _linkItem_link :: Text
  , _linkItem_description :: m ()
  }

linksList :: (Analytics t m, DomBuilder t m) => [LinkItem m] -> m ()
linksList links = elClass "dl" "links" $ for_ links $ \li -> do
  el "dt" $ extLink (_linkItem_link li) $ text $ _linkItem_title li
  el "dd" $ _linkItem_description li

guides :: (Analytics t m, DomBuilder t m) => Section m
guides = Section
  { _section_title = "Guides"
  , _section_content = do
    elClass "p" "description" $ text "These guides are a mixed bag - some API style documentation and some tutorial and walkthrough style resources."
    linksList
      [ LinkItem
        { _linkItem_title = "Reflex-Dom quick(ish) reference guide"
        , _linkItem_link = "https://github.com/reflex-frp/reflex-dom/blob/develop/Quickref.md"
        , _linkItem_description = text "A guide to typeclasses, widgets, and connecting to the real world."
        }
      , LinkItem
        { _linkItem_title = "A Beginner-friendly Reflex-DOM guide"
        , _linkItem_link = "https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md"
        , _linkItem_description = text "A back to basics explanation of FRP, and how to write GUI’s using Reflex."
        }
      , LinkItem
        { _linkItem_title = "Reflex quick(ish) reference guide"
        , _linkItem_link = "https://github.com/reflex-frp/reflex/blob/develop/Quickref.md"
        , _linkItem_description = text "Guide to typeclasses, function classification and organization, and connecting to the real world."
        }
      , LinkItem
        { _linkItem_title = "Functional Reactive Programming with Reflex"
        , _linkItem_link = "https://qfpl.io/projects/reflex/"
        , _linkItem_description = text "A 13-part, two-talk guide to learning and using Reflex by Dave Laing."
        }
      , LinkItem
        { _linkItem_title = "An Introduction to Brick+Reflex"
        , _linkItem_link = "https://hexagoxel.de/postsforpublish/posts/2017-10-30-brick-plus-reflex.html"
        , _linkItem_description = text "Guide to writing standalone applications (a commandline) with the reflex library."
        }
      , LinkItem
        { _linkItem_title = "CodeWorld & Reflex Integration"
        , _linkItem_link = "https://medium.com/@cdsmithus/functional-reactive-programming-with-reflex-and-codeworld-85495360f1b7"
        , _linkItem_description = text "Guide by Chris Smith on how to get started with cross-platform development using Reflex-DOM and CodeWorld."
        }
      , LinkItem
        { _linkItem_title = "Comprehensive Reflex Documentation (Release 0.5)"
        , _linkItem_link = "https://buildmedia.readthedocs.org/media/pdf/reflex-frp/latest/reflex-frp.pdf"
        , _linkItem_description = text "A 44 page complete walkthrough of the Reflex ecosystem and a collection of tutorials by Divam Narula."
        }
      , LinkItem
        { _linkItem_title = "Get Started with Reflex"
        , _linkItem_link = "http://haskell.vacationlabs.com/en/latest/docs/reflex/getting-started.html"
        , _linkItem_description = text "A Reflex Platform quick-start guide complete with a tutorial by Saurabh Nanda."
        }
      ]
  , _section_subsections = []
  }

workshops :: (Analytics t m, DomBuilder t m) => Section m
workshops = Section
  { _section_title = "Workshops"
  , _section_content = do
    linksList
      [ LinkItem
        { _linkItem_title = "The Reflex Architecture"
        , _linkItem_link = "https://yowconference.com/talks/ben-kolera/yow-lambda-jam-2019/the-reflex-architecture-combo-workshop-10032/"
        , _linkItem_description = text "A Reflex Dom workshop by Ben Kolera on writing frontend web applications in Haskell using Reflex. Some previous experience with FRP is recommended."
        }
      ]
  , _section_subsections = []
  }

presentations :: (Analytics t m, DomBuilder t m) => Section m
presentations = Section
  { _section_title = "Presentations"
  , _section_content = do
    linksList
      [ LinkItem
        { _linkItem_title = "7GUI’s with Reflex FRP"
        , _linkItem_link = "http://slides.com/mdrexl/7guis-reflex#/11"
        , _linkItem_description = text "A slides presentation by Moritz Drexel that walks through how to write GUI’s with Reflex."
        }
      , LinkItem
        { _linkItem_title = "Dabbling with Reflex FRP (and ghcjs)"
        , _linkItem_link = "https://emmanueltouzery.github.io/reflex-presentation/#29"
        , _linkItem_description = text "A slides presentation by Emmanuel Touzery on the workings of Reflex and it’s recommended uses."
        }
      ]
  , _section_subsections = []
  }

discussionForums :: (Analytics t m, DomBuilder t m) => Section m
discussionForums = Section
  { _section_title = "Discussion Forums"
  , _section_content = do
    linksList
      [ LinkItem
        { _linkItem_title = "Reflex Reddit Discussion"
        , _linkItem_link = "https://www.reddit.com/r/reflexfrp/"
        , _linkItem_description = text "A collection of links relating to the Reflex library, and discussions surrounding them."
        }
      , LinkItem
        { _linkItem_title = "Stack Overflow"
        , _linkItem_link = "https://stackoverflow.com/tags/reflex/hot?filter=all"
        , _linkItem_description = text "Discussion and question board for Reflex-FRP"
        }
      ]
  , _section_subsections = []
  }

downloads :: (Analytics t m, DomBuilder t m) => Section m
downloads = Section
  { _section_title = "Downloads"
  , _section_content = do
    linksList
      [ LinkItem
        { _linkItem_title = "Github Reflex-FRP"
        , _linkItem_link = "https://github.com/reflex-frp"
        , _linkItem_description = text "Pinned repositories"
        }
      , LinkItem
        { _linkItem_title = "Reflex"
        , _linkItem_link = "https://github.com/reflex-frp/reflex"
        , _linkItem_description = text "README, pull requests, and additional resources on Github."
        }
      , LinkItem
        { _linkItem_title = "Reflex-DOM"
        , _linkItem_link = "https://github.com/reflex-frp/reflex-dom"
        , _linkItem_description = text "README, pull requests, setup instructions, and additional resources on Github."
        }
      , LinkItem
        { _linkItem_title = "Reflex-VTY"
        , _linkItem_link = "https://github.com/reflex-frp/reflex-vty"
        , _linkItem_description = text "README, pull requests, how to build, and Cabal instructions."
        }
      , LinkItem
        { _linkItem_title = "Reflex Examples"
        , _linkItem_link = "https://github.com/reflex-frp/reflex-examples"
        , _linkItem_description = text "Download instructions, link to Reflex website page for examples."
        }
      , LinkItem
        { _linkItem_title = "Hackage Reflex Library"
        , _linkItem_link = "http://hackage.haskell.org/package/reflex"
        , _linkItem_description = text "Reflex documentation on Hackage."
        }
      ]
  , _section_subsections = []
  }

frpExtras :: (Analytics t m, DomBuilder t m) => Section m
frpExtras = Section
  { _section_title = "FRP Extras"
  , _section_content = do
    linksList
      [ LinkItem
        { _linkItem_title = "Functional Reactive Web Interfaces with GHCJS and sodium"
        , _linkItem_link = "http://weblog.luite.com/wordpress/?p=127"
        , _linkItem_description = text "A tutorial by Luite Stegeman."
        }
      , LinkItem
        { _linkItem_title = "WikiHaskell"
        , _linkItem_link = "https://wiki.haskell.org/Functional_Reactive_Programming"
        , _linkItem_description = text "Explanation of Functional Reactive Programming."
        }
      , LinkItem
        { _linkItem_title = "Functional Reactive Programming the book"
        , _linkItem_link = "https://livebook.manning.com/book/functional-reactive-programming/about-this-book/"
        , _linkItem_description = text "By Stephen Blackheath and Anthony Jones."
        }
      , LinkItem
        { _linkItem_title = "Explicitly Comprehensible Functional Reactive Programming"
        , _linkItem_link = "https://futureofcoding.org/papers/comprehensible-frp/comprehensible-frp.pdf"
        , _linkItem_description = text "A paper by Steven Krouse comparing The Elm Architecture and Reflex Ecosystem’s frameworks."
        }
      ]
  , _section_subsections = []
  }
