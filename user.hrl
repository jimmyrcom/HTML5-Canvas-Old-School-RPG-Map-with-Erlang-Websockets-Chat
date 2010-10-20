-record(state, 
        { maps = array:new(2,{default,dict:new()})
          , increment = 0
          , lookupByID = dict:new()
          , lookupByName = gb_trees:empty()
          , lookupByIP = gb_trees:empty()
          , banned = []
          , sock
        }).
-record(user, 
        {  x="0"
           , y="0"
           , id=0
           , user=""
           , map=0
           , lastMessage=u:munixtime()-3000
           , lastAction=u:munixtime()-3000
           , floodTest= [ 4000 || _ <- lists:seq(1,6)]
           , sprite="0"
           , ip
           , auth="0"
           , sock
           , pid
        }).
-record(simple,
        {  id=0
           , map=0
           , sock
        }).
