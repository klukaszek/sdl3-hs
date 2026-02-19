-- Rewrite relative links in README to point at the GitHub repository.
-- This keeps links working on GitHub Pages, where relative paths would
-- otherwise resolve under the site URL instead of the repo.

local repo_blob_base = "https://github.com/klukaszek/sdl3-hs/blob/main/"

local function is_external_or_anchor(target)
  return target:match("^https?://")
    or target:match("^mailto:")
    or target:match("^#")
    or target:match("^/")
end

function Link(el)
  local target = el.target
  if is_external_or_anchor(target) then
    return nil
  end

  el.target = repo_blob_base .. target
  return el
end
