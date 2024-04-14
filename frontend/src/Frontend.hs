{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings, RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Frontend where

import Control.Lens ((^.))
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.String
import Data.FileEmbed
import Language.Javascript.JSaddle (MonadJSM, eval, liftJSM, runJSM, JSVal, obj, jss, ToJSVal, JSM, fun, js, js1, jsg, js2, ghcjsPure, val)
import JSDOM (currentWindowUnchecked, currentDocumentUnchecked)
-- import Control.Monad.Trans.State (liftIO)

import Language.Javascript.JSaddle

import Control.Monad.IO.Class

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route

import Data.Text (Text)
import qualified Data.Map as Map
import Data.Map (Map)


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: "https://cdnjs.cloudflare.com/ajax/libs/tailwindcss/2.0.2/tailwind.min.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "script" ("type" =: "application/javascript" <> "src" =: "https://cdn.tailwindcss.com") blank
      elAttr "script" ("type" =: "application/javascript" <> "src" =: $(static "lib.js")) blank
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      -- el "h1" $ text "Welcome to Obelisk!"
      -- el "p" $ text $ T.pack commonStuff

      -- `prerender` and `prerender_` let you choose a widget to run on the server
      -- during prerendering and a different widget to run on the client with
      -- JavaScript. The following will generate a `blank` widget on the server and
      -- print "Hello, World!" on the client.
      prerender_ blank $ liftJSM $ void
        $ jsg ("window" :: T.Text)
        ^. js ("skeleton_lib" :: T.Text)
        ^. js1 ("log" :: T.Text) ("Hello, World!" :: T.Text)

      -- prerender_ blank $ void $ elDynHtml' "div" $ constDyn $(embedStringFile "frontend/src/test.html")

      elAttr "div" ("class" =: "bg-white") $ do
        el "header" $ do
          headerWidget
          heroWidget
          socialProofWidget
          featureSectionWidget
          testimonialsWidget
          pricingWidget
          faqWidget
          ctaWidget
          footerWidget

      return ()
  }

---------- nav menu ---------------------------------------------
headerWidget :: DomBuilder t m => m ()
headerWidget = do
  elAttr "header" ("class" =: "absolute inset-x-0 top-0 z-50") $ do
    elAttr "nav" ("class" =: "flex items-center justify-between p-6 lg:px-8" <> "aria-label" =: "Global") $ do
      elAttr "div" ("class" =: "flex lg:flex-1") $ do
        elAttr "a" ("href" =: "#" <> "class" =: "-m-1.5 p-1.5") $ do
          elAttr "span" ("class" =: "sr-only") $ text "Your Company"
          elAttr "img" ("src" =: "https://tailwindui.com/img/logos/mark.svg?color=indigo&shade=600" <> "alt" =: "" <> "class" =: "h-8 w-auto") blank
      elAttr "div" ("class" =: "flex lg:hidden") $ do
        elAttr "button" ("type" =: "button" <> "class" =: "-m-2.5 inline-flex items-center justify-center rounded-md p-2.5 text-gray-700") $ do
          elAttr "span" ("class" =: "sr-only") $ text "Open main menu"
          text "☰"  -- Simplified icon representation

      elAttr "div" ("class" =: "hidden lg:flex lg:gap-x-12") $ do
        navLink "Product"
        navLink "Features"
        navLink "Marketplace"
        navLink "Company"

      elAttr "div" ("class" =: "hidden lg:flex lg:flex-1 lg:justify-end") $ do
        elAttr "a" ("href" =: "#" <> "class" =: "text-sm font-semibold leading-6 text-gray-900") $ do
          text "Log in "
          elAttr "span" ("aria-hidden" =: "true") $ text "→"

    -- Mobile menu implementation
    elAttr "div" ("class" =: "lg:hidden" <> "role" =: "dialog" <> "aria-modal" =: "true") $ do
      elAttr "div" ("class" =: "fixed inset-0 z-50") blank
      elAttr "div" ("class" =: "fixed inset-y-0 right-0 z-50 w-full overflow-y-auto bg-white px-6 py-6 sm:max-w-sm sm:ring-1 sm:ring-gray-900/10") $ do
        elAttr "div" ("class" =: "flex items-center justify-between") $ do
          elAttr "a" ("href" =: "#" <> "class" =: "-m-1.5 p-1.5") $ do
            elAttr "span" ("class" =: "sr-only") $ text "Your Company"
            elAttr "img" ("src" =: "https://tailwindui.com/img/logos/mark.svg?color=indigo&shade=600" <> "alt" =: "" <> "class" =: "h-8 w-auto") blank
          elAttr "button" ("type" =: "button" <> "class" =: "-m-2.5 rounded-md p-2.5 text-gray-700") $ do
            elAttr "span" ("class" =: "sr-only") $ text "Close menu"
            text "✕"  -- Simplified close icon representation
        mobileMenuContent

navLink :: DomBuilder t m => Text -> m ()
navLink label = elAttr "a" ("href" =: "#" <> "class" =: "text-sm font-semibold leading-6 text-gray-900") $ text label

mobileMenuContent :: DomBuilder t m => m ()
mobileMenuContent = do
  elAttr "div" ("class" =: "mt-6 flow-root") $ do
    elAttr "div" ("class" =: "-my-6 divide-y divide-gray-500/10") $ do
      menuItem "Product"
      menuItem "Features"
      menuItem "Marketplace"
      menuItem "Company"
      elAttr "div" ("class" =: "py-6") $ do
        menuItem "Log in"

menuItem :: DomBuilder t m => Text -> m ()
menuItem name = elAttr "a" ("href" =: "#" <> "class" =: "-mx-3 block rounded-lg px-3 py-2 text-base font-semibold leading-7 text-gray-900 hover:bg-gray-50") $ text name
-----------------------------------------------


-- hero section ------------------------------
heroWidget :: DomBuilder t m => m ()
heroWidget = elClass "main" "isolate" $ do
  heroSection

heroSection :: DomBuilder t m => m ()
heroSection = elClass "div" "relative pt-14" $ do
  -- Background decorative elements
  elAttr "div" ("class" =: "absolute inset-x-0 -top-40 -z-10 transform-gpu overflow-hidden blur-3xl sm:-top-80" <> "aria-hidden" =: "true") $ do
    elAttr "div" ("class" =: "relative left-[calc(50%-11rem)] aspect-[1155/678] w-[36.125rem] -translate-x-1/2 rotate-[30deg] bg-gradient-to-tr from-[#ff80b5] to-[#9089fc] opacity-30 sm:left-[calc(50%-30rem)] sm:w-[72.1875rem]") $ blank

  -- Content block
  elClass "div" "py-24 sm:py-32" $ do
    elClass "div" "mx-auto max-w-7xl px-6 lg:px-8" $ do
      elClass "div" "mx-auto max-w-2xl text-center" $ do
        elClass "h1" "text-4xl font-bold tracking-tight text-gray-900 sm:text-6xl" $ text "Deploy to the cloud with confidence"
        elClass "p" "mt-6 text-lg leading-8 text-gray-600" $ text "Anim aute id magna aliqua ad ad non deserunt sunt. Qui irure qui lorem cupidatat commodo. Elit sunt amet fugiat veniam occaecat fugiat aliqua."
        elClass "div" "mt-10 flex items-center justify-center gap-x-6" $ do
          elAttr "a" ("href" =: "#" <> "class" =: "rounded-md bg-indigo-600 px-3.5 py-2.5 text-sm font-semibold text-white shadow-sm hover:bg-indigo-500 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-indigo-600") $ text "Get started"
          elAttr "a" ("href" =: "#" <> "class" =: "text-sm font-semibold leading-6 text-gray-900") $ do
            text "Learn more "
            elAttr "span" ("aria-hidden" =: "true") $ text "→"

      elClass "div" "mt-16 flow-root sm:mt-24" $ do
        elClass "div" "-m-2 rounded-xl bg-gray-900/5 p-2 ring-1 ring-inset ring-gray-900/10 lg:-m-4 lg:rounded-2xl lg:p-4" $ do
          elAttr "img" ("src" =: "https://tailwindui.com/img/component-images/project-app-screenshot.png" <> "alt" =: "App screenshot" <> "width" =: "2432" <> "height" =: "1442" <> "class" =: "rounded-md shadow-2xl ring-1 ring-gray-900/10") blank

  -- Additional background element
  elAttr "div" ("class" =: "absolute inset-x-0 top-[calc(100%-13rem)] -z-10 transform-gpu overflow-hidden blur-3xl sm:top-[calc(100%-30rem)]" <> "aria-hidden" =: "true") $ do
    elAttr "div" ("class" =: "relative left-[calc(50%+3rem)] aspect-[1155/678] w-[36.125rem] -translate-x-1/2 bg-gradient-to-tr from-[#ff80b5] to-[#9089fc] opacity-30 sm:left-[calc(50%+36rem)] sm:w-[72.1875rem]") $ blank

--------------------------------------------

-- social proof ------------------------------
socialProofWidget :: DomBuilder t m => m ()
socialProofWidget = do
  elClass "div" "mx-auto max-w-7xl px-6 lg:px-8" $ do
    elClass "div" "mx-auto grid max-w-lg grid-cols-4 items-center gap-x-8 gap-y-12 sm:max-w-xl sm:grid-cols-6 sm:gap-x-10 sm:gap-y-14 lg:mx-0 lg:max-w-none lg:grid-cols-5" $ do
      logo "https://tailwindui.com/img/logos/158x48/transistor-logo-gray-900.svg" "Transistor" "158" "48" "col-span-2 max-h-12 w-full object-contain lg:col-span-1"
      logo "https://tailwindui.com/img/logos/158x48/reform-logo-gray-900.svg" "Reform" "158" "48" "col-span-2 max-h-12 w-full object-contain lg:col-span-1"
      logo "https://tailwindui.com/img/logos/158x48/tuple-logo-gray-900.svg" "Tuple" "158" "48" "col-span-2 max-h-12 w-full object-contain lg:col-span-1"
      logo "https://tailwindui.com/img/logos/158x48/savvycal-logo-gray-900.svg" "SavvyCal" "158" "48" "col-span-2 max-h-12 w-full object-contain sm:col-start-2 lg:col-span-1"
      logo "https://tailwindui.com/img/logos/158x48/statamic-logo-gray-900.svg" "Statamic" "158" "48" "col-span-2 col-start-2 max-h-12 w-full object-contain sm:col-start-auto lg:col-span-1"

    elClass "div" "mt-16 flex justify-center" $ do
      elClass "p" "relative rounded-full px-4 py-1.5 text-sm leading-6 text-gray-600 ring-1 ring-inset ring-gray-900/10 hover:ring-gray-900/20" $ do
        elClass "span" "hidden md:inline" $ text "Transistor saves up to $40,000 per year, per employee by working with us."
        elAttr "a" ("href" =: "#" <> "class" =: "font-semibold text-indigo-600") $ do
          elClass "span" "absolute inset-0" $ blank -- for clickable area coverage
          text "Read our case study "
          elAttr "span" ("aria-hidden" =: "true") $ text "→"

logo :: DomBuilder t m => Text -> Text -> Text -> Text -> Text -> m ()
logo src alt width height classes = elAttr "img" ("src" =: src <> "alt" =: alt <> "width" =: width <> "height" =: height <> "class" =: classes) blank
----------------------------

-------- feature section ---------------------------

featureSectionWidget :: DomBuilder t m => m ()
featureSectionWidget = do
  elClass "div" "mx-auto mt-32 max-w-7xl px-6 sm:mt-56 lg:px-8" $ do
    elClass "div" "mx-auto max-w-2xl lg:text-center" $ do
      elClass "h2" "text-base font-semibold leading-7 text-indigo-600" $ text "Deploy faster"
      elClass "p" "mt-2 text-3xl font-bold tracking-tight text-gray-900 sm:text-4xl" $ text "Everything you need to deploy your app"
      elClass "p" "mt-6 text-lg leading-8 text-gray-600" $ text "Quis tellus eget adipiscing convallis sit sit eget aliquet quis. Suspendisse eget egestas a elementum pulvinar et feugiat blandit at. In mi viverra elit nunc."

    elClass "div" "mx-auto mt-16 max-w-2xl sm:mt-20 lg:mt-24 lg:max-w-4xl" $ do
      elClass "dl" "grid max-w-xl grid-cols-1 gap-x-8 gap-y-10 lg:max-w-none lg:grid-cols-2 lg:gap-y-16" $ do
        featureItem "Push to deploy" "M12 16.5V9.75m0 0l3 3m-3-3l-3 3M6.75 19.5a4.5 4.5 0 01-1.41-8.775 5.25 5.25 0 0110.233-2.33 3 3 0 013.758 3.848A3.752 3.752 0 0118 19.5H6.75z" "Morbi viverra dui mi arcu sed. Tellus semper adipiscing suspendisse semper morbi. Odio urna massa nunc massa."
        featureItem "SSL certificates" "M16.5 10.5V6.75a4.5 4.5 0 10-9 0v3.75m-.75 11.25h10.5a2.25 2.25 0 002.25-2.25v-6.75a2.25 2.25 0 00-2.25-2.25H6.75a2.25 2.25 0 00-2.25 2.25v6.75a2.25 2.25 0 002.25 2.25z" "Sit quis amet rutrum tellus ullamcorper ultricies libero dolor eget. Sem sodales gravida quam turpis enim lacus amet."
        featureItem "Simple queues" "M16.023 9.348h4.992v-.001M2.985 19.644v-4.992m0 0h4.992m-4.993 0l3.181 3.183a8.25 8.25 0 0013.803-3.7M4.031 9.865a8.25 8.25 0 0113.803-3.7l3.181 3.182m0-4.991v4.99" "Quisque est vel vulputate cursus. Risus proin diam nunc commodo. Lobortis auctor congue commodo diam neque."
        featureItem "Advanced security" "M7.864 4.243A7.5 7.5 0 0119.5 10.5c0 2.92-.556 5.709-1.568 8.268M5.742 6.364A7.465 7.465 0 004.5 10.5a7.464 7.464 0 01-1.15 3.993m1.989 3.559A11.209 11.209 0 008.25 10.5a3.75 3.75 0 117.5 0c0 .527-.021 1.049-.064 1.565M12 10.5a14.94 14.94 0 01-3.6 9.75m6.633-4.596a18.666 18.666 0 01-2.485 5.33" "Arcu egestas dolor vel iaculis in ipsum mauris. Tincidunt mattis aliquet hac quis. Id hac maecenas ac donec pharetra eget."

featureItem :: DomBuilder t m => Text -> Text -> Text -> m ()
featureItem title iconPath description = do
  elClass "div" "relative pl-16" $ do
    elClass "dt" "text-base font-semibold leading-7 text-gray-900" $ do
      elClass "div" "absolute left-0 top-0 flex h-10 w-10 items-center justify-center rounded-lg bg-indigo-600" $ do
        elAttr "svg" ("class" =: "h-6 w-6 text-white" <> "fill" =: "none" <> "viewBox" =: "0 0 24 24" <> "stroke-width" =: "1.5" <> "stroke" =: "currentColor" <> "aria-hidden" =: "true") $ do
          elAttr "path" ("stroke-linecap" =: "round" <> "stroke-linejoin" =: "round" <> "d" =: iconPath) blank
      text title
    elClass "dd" "mt-2 text-base leading-7 text-gray-600" $ text description

-----------------------------------------------


-------------- testimonials section ------------------

testimonialsWidget :: DomBuilder t m => m ()
testimonialsWidget = do
  elClass "div" "mx-auto mt-32 max-w-7xl sm:mt-56 sm:px-6 lg:px-8" $ do
    elClass "div" "relative overflow-hidden bg-gray-900 px-6 py-20 shadow-xl sm:rounded-3xl sm:px-10 sm:py-24 md:px-12 lg:px-20" $ do
      elAttr "img" ("class" =: "absolute inset-0 h-full w-full object-cover brightness-150 saturate-0" <>
                    "src" =: "https://images.unsplash.com/photo-1601381718415-a05fb0a261f3?ixid=MXwxMjA3fDB8MHxwcm9maWxlLXBhZ2V8ODl8fHxlbnwwfHx8&ixlib=rb-1.2.1&auto=format&fit=crop&w=1216&q=80" <>
                    "alt" =: "") blank
      elClass "div" "absolute inset-0 bg-gray-900/90 mix-blend-multiply" blank
      decorativeElement
      responsiveDecorativeElement
      testimonialContent

decorativeElement :: DomBuilder t m => m ()
decorativeElement = elClass "div" "absolute -left-80 -top-56 transform-gpu blur-3xl" $ do
  elAttr "div" ("class" =: "aspect-[1097/845] w-[68.5625rem] bg-gradient-to-r from-[#ff4694] to-[#776fff] opacity-[0.45]" <>
                "style" =: "clip-path: polygon(74.1% 44.1%, 100% 61.6%, 97.5% 26.9%, 85.5% 0.1%, 80.7% 2%, 72.5% 32.5%, 60.2% 62.4%, 52.4% 68.1%, 47.5% 58.3%, 45.2% 34.5%, 27.5% 76.7%, 0.1% 64.9%, 17.9% 100%, 27.6% 76.8%, 76.1% 97.7%, 74.1% 44.1%)") blank

responsiveDecorativeElement :: DomBuilder t m => m ()
responsiveDecorativeElement = elClass "div" "hidden md:absolute md:bottom-16 md:left-[50rem] md:block md:transform-gpu md:blur-3xl" $ do
  elAttr "div" ("class" =: "aspect-[1097/845] w-[68.5625rem] bg-gradient-to-r from-[#ff4694] to-[#776fff] opacity-25" <>
                "style" =: "clip-path: polygon(74.1% 44.1%, 100% 61.6%, 97.5% 26.9%, 85.5% 0.1%, 80.7% 2%, 72.5% 32.5%, 60.2% 62.4%, 52.4% 68.1%, 47.5% 58.3%, 45.2% 34.5%, 27.5% 76.7%, 0.1% 64.9%, 17.9% 100%, 27.6% 76.8%, 76.1% 97.7%, 74.1% 44.1%)") blank

testimonialContent :: DomBuilder t m => m ()
testimonialContent = elClass "div" "relative mx-auto max-w-2xl lg:mx-0" $ do
  elAttr "img" ("src" =: "https://tailwindui.com/img/logos/workcation-logo-white.svg" <> "alt" =: "" <> "class" =: "h-12 w-auto") blank
  el "figure" $ do
    elClass "blockquote" "mt-6 text-lg font-semibold text-white sm:text-xl sm:leading-8" $ do
      el "p" $ text "“Amet amet eget scelerisque tellus sit neque faucibus non eleifend. Integer eu praesent at a. Ornare arcu gravida natoque erat et cursus tortor consequat at. Vulputate gravida sociis enim nullam ultricies habitant malesuada lorem ac.”"
    elClass "figcaption" "mt-6 text-base text-white" $ do
      elClass "div" "font-semibold" $ text "Judith Black"
      elClass "div" "mt-1" $ text "CEO of Tuple"

----------------------------------------------


------------- pricing section --------------------------------

pricingWidget :: DomBuilder t m => m ()
pricingWidget = do
  elClass "div" "py-24 sm:pt-48" $ do
    elClass "div" "mx-auto max-w-7xl px-6 lg:px-8" $ do
      elClass "div" "mx-auto max-w-4xl text-center" $ do
        elClass "h2" "text-base font-semibold leading-7 text-indigo-600" $ text "Pricing"
        elClass "p" "mt-2 text-4xl font-bold tracking-tight text-gray-900 sm:text-5xl" $ text "Pricing plans for teams of all sizes"
      elClass "p" "mx-auto mt-6 max-w-2xl text-center text-lg leading-8 text-gray-600" $
        text "Distinctio et nulla eum soluta et neque labore quibusdam. Saepe et quasi iusto modi velit ut non voluptas in. Explicabo id ut laborum."

      elClass "div" "isolate mx-auto mt-16 grid max-w-md grid-cols-1 gap-y-8 sm:mt-20 lg:mx-0 lg:max-w-none lg:grid-cols-3" $ do
        pricingPlan "Freelancer" "$24" "/month"
          ["5 products", "Up to 1,000 subscribers", "Basic analytics", "48-hour support response time"]
          "Buy plan"
        pricingPlan "Startup" "$32" "/month"
          ["25 products", "Up to 10,000 subscribers", "Advanced analytics", "24-hour support response time", "Marketing automations"]
          "Buy plan"
        pricingPlan "Enterprise" "$48" "/month"
          ["Unlimited products", "Unlimited subscribers", "Advanced analytics", "1-hour, dedicated support response time", "Marketing automations"]
          "Buy plan"

pricingPlan :: DomBuilder t m => Text -> Text -> Text -> [Text] -> Text -> m ()
pricingPlan plan price per features buttonText = do
  elClass "div" "flex flex-col justify-between rounded-3xl bg-white p-8 ring-1 ring-gray-200 xl:p-10 lg:mt-8 lg:rounded-r-none" $ do
    el "div" $ do
      elClass "h3" "text-lg font-semibold leading-8 text-gray-900" $ text plan
      elClass "p" "mt-4 text-sm leading-6 text-gray-600" $ text "The essentials to provide your best work for clients."
      elClass "p" "mt-6 flex items-baseline gap-x-1" $ do
        elClass "span" "text-4xl font-bold tracking-tight text-gray-900" $ text price
        elClass "span" "text-sm font-semibold leading-6 text-gray-600" $ text per
      elClass "ul" "mt-8 space-y-3 text-sm leading-6 text-gray-600" $ mapM_ (elClass "li" "flex gap-x-3" . pricingItem) features
    elAttr "a" ("href" =: "#" <> "class" =: "mt-8 block rounded-md py-2 px-3 text-center text-sm font-semibold leading-6 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-indigo-600 text-indigo-600 ring-1 ring-inset ring-indigo-200 hover:ring-indigo-300") $ text buttonText

pricingItem :: DomBuilder t m => Text -> m ()
pricingItem feature = do
  elClass "svg" "h-6 w-5 flex-none text-indigo-600" $ do
    elAttr "path" ("fill-rule" =: "evenodd" <> "d" =: "M16.704 4.153a.75.75 0 01.143 1.052l-8 10.5a.75.75 0 01-1.127.075l-4.5-4.5a.75.75 0 011.06-1.06l3.894 3.893 7.48-9.817a.75.75 0 011.05-.143z" <> "clip-rule" =: "evenodd") blank
  text feature

------------------------------------------

------------ FAQ section -------------------------

faqWidget :: DomBuilder t m => m ()
faqWidget = do
  elClass "div" "mx-auto max-w-2xl divide-y divide-gray-900/10 px-6 pb-8 sm:pb-24 sm:pt-12 lg:max-w-7xl lg:px-8 lg:pb-32" $ do
    elClass "h2" "text-2xl font-bold leading-10 tracking-tight text-gray-900" $ text "Frequently asked questions"
    elClass "dl" "mt-10 space-y-8 divide-y divide-gray-900/10" $ do
      faqItem "What's the best thing about Switzerland?"
              "I don't know, but the flag is a big plus. Lorem ipsum dolor sit amet consectetur adipisicing elit. Quas cupiditate laboriosam fugiat."

-- Helper function to create FAQ items
faqItem :: DomBuilder t m => Text -> Text -> m ()
faqItem question answer = do
  elClass "div" "pt-8 lg:grid lg:grid-cols-12 lg:gap-8" $ do
    elClass "dt" "text-base font-semibold leading-7 text-gray-900 lg:col-span-5" $ text question
    elClass "dd" "mt-4 lg:col-span-7 lg:mt-0" $
      elClass "p" "text-base leading-7 text-gray-600" $ text answer

--------------------------------------------------

----------------------- CTA section --------------

ctaWidget :: DomBuilder t m => m ()
ctaWidget = do
  elClass "main" "isolate" $ do
    elClass "div" "relative -z-10 mt-32 px-6 lg:px-8" $ do
      -- Background decorative element
      elAttr "div" ("class" =: "absolute inset-x-0 top-1/2 -z-10 flex -translate-y-1/2 transform-gpu justify-center overflow-hidden blur-3xl sm:bottom-0 sm:right-[calc(50%-6rem)] sm:top-auto sm:translate-y-0 sm:transform-gpu sm:justify-end" <> "aria-hidden" =: "true") $
        elAttr "div" ("class" =: "aspect-[1108/632] w-[69.25rem] flex-none bg-gradient-to-r from-[#ff80b5] to-[#9089fc] opacity-25" <>
                      "style" =: "clip-path: polygon(73.6% 48.6%, 91.7% 88.5%, 100% 53.9%, 97.4% 18.1%, 92.5% 15.4%, 75.7% 36.3%, 55.3% 52.8%, 46.5% 50.9%, 45% 37.4%, 50.3% 13.1%, 21.3% 36.2%, 0.1% 0.1%, 5.4% 49.1%, 21.4% 36.4%, 58.9% 100%, 73.6% 48.6%)") blank

      -- Main Content Area
      elClass "div" "mx-auto max-w-2xl text-center" $ do
        elClass "h2" "text-3xl font-bold tracking-tight text-gray-900 sm:text-4xl" $ do
          text "Boost your productivity."
          el "br" blank
          text "Start using our app today."
        elClass "p" "mx-auto mt-6 max-w-xl text-lg leading-8 text-gray-600" $
          text "Incididunt sint fugiat pariatur cupidatat consectetur sit cillum anim id veniam aliqua proident excepteur commodo do ea."
        elClass "div" "mt-10 flex items-center justify-center gap-x-6" $ do
          elAttr "a" ("href" =: "#" <> "class" =: "rounded-md bg-indigo-600 px-3.5 py-2.5 text-sm font-semibold text-white shadow-sm hover:bg-indigo-500 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-indigo-600") $
            text "Get started"
          elAttr "a" ("href" =: "#" <> "class" =: "text-sm font-semibold leading-6 text-gray-900") $ do
            text "Learn more "
            el "span" (text "→")

      -- Additional decorative background
      elAttr "div" ("class" =: "absolute left-1/2 right-0 top-full -z-10 hidden -translate-y-1/2 transform-gpu overflow-hidden blur-3xl sm:block" <> "aria-hidden" =: "true") $
        elAttr "div" ("class" =: "aspect-[1155/678] w-[72.1875rem] bg-gradient-to-tr from-[#ff80b5] to-[#9089fc] opacity-30" <>
                      "style" =: "clip-path: polygon(74.1% 44.1%, 100% 61.6%, 97.5% 26.9%, 85.5% 0.1%, 80.7% 2%, 72.5% 32.5%, 60.2% 62.4%, 52.4% 68.1%, 47.5% 58.3%, 45.2% 34.5%, 27.5% 76.7%, 0.1% 64.9%, 17.9% 100%, 27.6% 76.8%, 76.1% 97.7%, 74.1% 44.1%)") blank
--------------------------------------------------


-------------------------- footer section ---------------------------

footerWidget :: DomBuilder t m => m ()
footerWidget = do
  elClass "div" "mx-auto mt-32 max-w-7xl px-6 lg:px-8" $ do
    elAttr "footer" ("aria-labelledby" =: "footer-heading" <> "class" =: "relative border-t border-gray-900/10 py-24 sm:mt-56 sm:py-32") $ do
      elAttr "h2" ("id" =: "footer-heading" <> "class" =: "sr-only") $ text "Footer"
      elClass "div" "xl:grid xl:grid-cols-3 xl:gap-8" $ do
        elAttr "img" ("src" =: "https://tailwindui.com/img/logos/mark.svg?color=indigo&shade=600" <> "class" =: "h-7" <> "alt" =: "Company name") blank
        elClass "div" "mt-16 grid grid-cols-2 gap-8 xl:col-span-2 xl:mt-0" $ do
          footerColumn "Solutions" ["Hosting", "Data Services", "Uptime Monitoring", "Enterprise Services"]
          footerColumn "Support" ["Pricing", "Documentation", "Guides", "API Reference"]
          footerColumn "Company" ["About", "Blog", "Jobs", "Press", "Partners"]
          footerColumn "Legal" ["Claim", "Privacy", "Terms"]

footerColumn :: DomBuilder t m => Text -> [Text] -> m ()
footerColumn title links = do
  el "div" $ do
    elClass "h3" "text-sm font-semibold leading-6 text-gray-900" $ text title
    elClass "ul" "mt-6 space-y-4" $ mapM_ (\link -> el "li" $ elAttr "a" ("href" =: "#" <> "class" =: "text-sm leading-6 text-gray-600 hover:text-gray-900") $ text link) links

---------------------------------------------------------
