-type key()              :: any() | '_' .
-type value()            :: binary() | '_'.
-type type()             :: any() | '_'.
-type scope()            :: any() | '_'.
-type seconds()          :: non_neg_integer() | '_'.
-type rec_ref()          :: reference() | '_'.
-type ttl()              :: seconds() | '_'.
-type time_stamp()       :: seconds() | '_'.

-type cache_result()     :: {key(), value(), type(), scope(), 
                             create_tm(), last_update_tm(), inactivate_tm(), 
			     rec_ref()}.

-type cache_insert()     :: {key(), value()} | {key(), value(), ttl()} | 
			    {key(), value(), type(), scope()} | 
			    {key(), value(), type(), scope(), ttl()}.


-type change_list()      :: {changelist, 
			     {Add::[cache_result()], 
			      Update::[cache_result()], 
			      Delete::[cache_result()]}}.

-type change_lists()      ::[{type() | scope(),
			      {Add::[cache_result()], 
			       Update::[cache_result()], 
			       Delete::[cache_result()]}}].


-type create_tm()        :: time_stamp().
-type last_update_tm()   :: time_stamp().
-type inactivate_tm()    :: time_stamp() | undefined.
-type key_to_value()     :: #key_to_value{}.
