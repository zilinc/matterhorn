-- We can define whatever helper functions we want here
function cmd(s)
   return io.popen(s):read()
end

-- The 'connection' function takes a table of values, which are mostly
-- supposed to be strings
connection {
   -- The username
   user = "<username>",
   -- The hostname
   host = "<hostname>",
   -- The team name
   team = "<team name>",
   -- The port, which is probably 443
   port = 443,
   -- And the password, which we get here by shelling out
   pass = cmd("security find-generic-password -s <password name> -w"),
}
