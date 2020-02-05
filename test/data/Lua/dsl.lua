html {
    body {
      h1 "Welcome to my Lua site",
      a {
        href = "http://leafo.net",
        "Go home"
      }
    }
  }

  print "hello" --> print("hello")
my_function { 1,2,3 } --> my_function({1,2,3})

-- whitespace isn't needed, these also work:

print"hello" --> print("hello")
my_function{ 1,2,3 } --> my_function({1,2,3})

tonumber "1234" + 5 -- > tonumber("1234") + 5

match "/post-comment" {
    GET = function ()
      -- render the form
    end,
  
    POST = function ()
      -- save to database
    end
  }

match("/post-comment")({ ... })

local function match(path)
    print("match:", path)
  
    return function(params)
      print("params:", params)
      -- both path and params are now availble for use here
    end
  end

  local function run_with_env(env, fn, ...)
    setfenv(fn, env)
    fn(...)
  end

  local dsl_env = {
    move = function(x,y)
      print("I moved to", x, y)
    end,
  
    speak = function(message)
      print("I said", message)
    end
  }
  
  run_with_env(dsl_env, function()
    move(10, 10)
    speak("I am hungry!")
  end)

  html {
    body {
      h1 "Welcome to my Lua site",
      a {
        href = "http://leafo.net",
        "Go home"
      }
    }
  }

  render_html(function()
    return div {
      img { src = "http://leafo.net/hi" }
    }
  end) -- > <div><img src="http://leafo.net/hi" /></div>

  local function render_html(fn)
    setfenv(fn, setmetatable({}, {
      __index = function(self, tag_name)
        return function(opts)
          return build_tag(tag_name, opts)
        end
      end
    }))
  
    return fn()
  end

  local void_tags = {
    img = true,
    -- etc...
  }

  local function append_all(buffer, ...)
    for i=1,select("#", ...) do
      table.insert(buffer, (select(i, ...)))
    end
  end
  
  -- example:
  --   local buffer = {}
  --   append_all(buffer, "a", "b", c)
  -- buffer now is {"a", "b", "c"}

  local function build_tag(tag_name, opts)
    local buffer = {"<", tag_name}
    if type(opts) == "table" then
      for k,v in pairs(opts) do
        if type(k) ~= "number" then
          append_all(buffer, " ", k, '="', v, '"')
        end
      end
    end
  
    if void_tags[tag_name] then
      append_all(buffer, " />")
    else
      append_all(buffer, ">")
      if type(opts) == "table" then
        append_all(buffer, unpack(opts))
      else
        append_all(buffer, opts)
      end
      append_all(buffer, "</", tag_name, ">")
    end
  
    return table.concat(buffer)
  end

  for k,v in pairs(opts) do
    if type(k) ~= "number" then
      -- access hash table key and values
    end
  end

  append_all(buffer, unpack(opts))

  local unsafe_text = [[<script type="text/javascript">alert('hacked!')</script>]]

render_html(function()
  return div(unsafe_text)
end)

-- should not return a functional script tag:
-- <div>&lt;script type=&quot;text/javascript&quot;&gt;alert('hacked!')&lt;/script&gt;</div>
