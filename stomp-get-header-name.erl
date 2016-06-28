5>
5> eunit:test(fun stomp_tests:basic_connection_test/0).
(<0.58.0>) call stomp:get_header_name([],"heart-beat:0,0\nsession:ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n")
(<0.58.0>) call stomp:get_header_name("h","eart-beat:0,0\nsession:ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n")
(<0.58.0>) call stomp:get_header_name("he","art-beat:0,0\nsession:ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n")
(<0.58.0>) call stomp:get_header_name("hea","rt-beat:0,0\nsession:ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n")
(<0.58.0>) call stomp:get_header_name("hear","t-beat:0,0\nsession:ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n")
(<0.58.0>) call stomp:get_header_name("heart","-beat:0,0\nsession:ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n")
(<0.58.0>) call stomp:get_header_name("heart-","beat:0,0\nsession:ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n")
(<0.58.0>) call stomp:get_header_name("heart-b","eat:0,0\nsession:ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n")
(<0.58.0>) call stomp:get_header_name("heart-be","at:0,0\nsession:ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n")
(<0.58.0>) call stomp:get_header_name("heart-bea","t:0,0\nsession:ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n")
(<0.58.0>) call stomp:get_header_name("heart-beat",":0,0\nsession:ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n")
(<0.58.0>) returned from stomp:get_header_name/2 -> {"heart-beat",
                                                     "0,0\nsession:ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"heart-beat",
                                                     "0,0\nsession:ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"heart-beat",
                                                     "0,0\nsession:ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"heart-beat",
                                                     "0,0\nsession:ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"heart-beat",
                                                     "0,0\nsession:ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"heart-beat",
                                                     "0,0\nsession:ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"heart-beat",
                                                     "0,0\nsession:ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"heart-beat",
                                                     "0,0\nsession:ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"heart-beat",
                                                     "0,0\nsession:ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"heart-beat",
                                                     "0,0\nsession:ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"heart-beat",
                                                     "0,0\nsession:ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n"}
(<0.58.0>) call stomp:get_header_name([],"session:ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n")
(<0.58.0>) call stomp:get_header_name("s","ession:ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n")
(<0.58.0>) call stomp:get_header_name("se","ssion:ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n")
(<0.58.0>) call stomp:get_header_name("ses","sion:ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n")
(<0.58.0>) call stomp:get_header_name("sess","ion:ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n")
(<0.58.0>) call stomp:get_header_name("sessi","on:ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n")
(<0.58.0>) call stomp:get_header_name("sessio","n:ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n")
(<0.58.0>) call stomp:get_header_name("session",":ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n")
(<0.58.0>) returned from stomp:get_header_name/2 -> {"session",
                                                     "ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"session",
                                                     "ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"session",
                                                     "ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"session",
                                                     "ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"session",
                                                     "ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"session",
                                                     "ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"session",
                                                     "ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"session",
                                                     "ID:x3.local-64932-1375438680738-2:11\nserver:ActiveMQ/5.8.0\nversion:1.0\n"}
(<0.58.0>) call stomp:get_header_name([],"server:ActiveMQ/5.8.0\nversion:1.0\n")
(<0.58.0>) call stomp:get_header_name("s","erver:ActiveMQ/5.8.0\nversion:1.0\n")
(<0.58.0>) call stomp:get_header_name("se","rver:ActiveMQ/5.8.0\nversion:1.0\n")
(<0.58.0>) call stomp:get_header_name("ser","ver:ActiveMQ/5.8.0\nversion:1.0\n")
(<0.58.0>) call stomp:get_header_name("serv","er:ActiveMQ/5.8.0\nversion:1.0\n")
(<0.58.0>) call stomp:get_header_name("serve","r:ActiveMQ/5.8.0\nversion:1.0\n")
(<0.58.0>) call stomp:get_header_name("server",":ActiveMQ/5.8.0\nversion:1.0\n")
(<0.58.0>) returned from stomp:get_header_name/2 -> {"server",
                                                     "ActiveMQ/5.8.0\nversion:1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"server",
                                                     "ActiveMQ/5.8.0\nversion:1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"server",
                                                     "ActiveMQ/5.8.0\nversion:1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"server",
                                                     "ActiveMQ/5.8.0\nversion:1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"server",
                                                     "ActiveMQ/5.8.0\nversion:1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"server",
                                                     "ActiveMQ/5.8.0\nversion:1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"server",
                                                     "ActiveMQ/5.8.0\nversion:1.0\n"}
(<0.58.0>) call stomp:get_header_name([],"version:1.0\n")
(<0.58.0>) call stomp:get_header_name("v","ersion:1.0\n")
(<0.58.0>) call stomp:get_header_name("ve","rsion:1.0\n")
(<0.58.0>) call stomp:get_header_name("ver","sion:1.0\n")
(<0.58.0>) call stomp:get_header_name("vers","ion:1.0\n")
(<0.58.0>) call stomp:get_header_name("versi","on:1.0\n")
(<0.58.0>) call stomp:get_header_name("versio","n:1.0\n")
(<0.58.0>) call stomp:get_header_name("version",":1.0\n")
(<0.58.0>) returned from stomp:get_header_name/2 -> {"version","1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"version","1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"version","1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"version","1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"version","1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"version","1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"version","1.0\n"}
(<0.58.0>) returned from stomp:get_header_name/2 -> {"version","1.0\n"}
  Test passed.
ok
6>
