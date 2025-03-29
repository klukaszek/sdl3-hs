import Foreign.Marshal.Utils (with)
import SDL


main :: IO ()
main = do
  let rect1 = SDLRect 0 0 10 10
      rect2 = SDLRect 5 5 10 10
      point = SDLPoint 5 5

  sdlLog $ "Point in rect1: " ++ show (sdlPointInRect point rect1)
  sdlLog $ "Rect1 empty: " ++ show (sdlRectEmpty rect1)
  sdlLog $ "Rects equal: " ++ show (sdlRectsEqual rect1 rect2)

  -- Cast our rectangles to pointers and pass them to the sdlHasRectIntersection function
  intersects <- with rect1 $ \rect1Ptr -> 
                with rect2 $ \rect2Ptr -> 
                sdlHasRectIntersection rect1Ptr rect2Ptr
  sdlLog $ "Rectangles intersect: " ++ show intersects
