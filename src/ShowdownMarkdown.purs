module ShowdownMarkdown (markdownToHtml) where

-- TODO: seems unsafe - does showdown throw errors?
foreign import markdownToHtml :: String -> String
