% key_to_value is an ordered_set table
-record (key_to_value,  
	 {update_map               :: {time_stamp(), key()} | {any(), '_'},
	  key                      :: key(),
	  type                     :: type(),
	  scope                    :: scope(),
	  value                    :: value(), 
	  create_tm                :: time_stamp(),
	  last_update              :: time_stamp(),
	  inactive_tm              :: time_stamp(),
	  ttl_secs                 :: ttl(), 
	  ref                      :: rec_ref()
	 }).

-record (session,            
	 {key                       ::reference(),
	  last_scope                :: integer(),
	  create                    :: integer(),
	  last_access               :: integer()
	 }).

-record (ttl,           
	 {key                      :: key(), 
	  timer_ref                :: reference()
	 }).

-record (stats,           
	 {key                      :: atom(), 
	  value                    :: any()
	 }).


