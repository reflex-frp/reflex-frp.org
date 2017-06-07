import Reflex.Dom
import Frontend.App
import Common.Route

main :: IO ()
main = mainWidgetWithHead siteHead $ siteBody Route_Home
