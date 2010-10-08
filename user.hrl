-record(user,
        { sock
         ,x="0"
         ,y="0"
         ,lastMessage=0
         ,lastAction=0
         ,ip
         ,id
         ,pid
        }).
-record(simple,
        { user
         , sock
          , x = "0"
          , y = "0"
        }).
