Jcache
====



##Erlang, Distributable, In-Memory Cache


###Features
* HTTP Interface supporting all persistance and lookup functions
  * Long Polling support
  * GETS return JSON
* Erlang API
* Keys are mapped to Value, Type, and Scope where Type and Scope are arbitrarily
  named meta-data tags that allow for additional categorization of Keys
* ACIDic transactions over multiple Keys, Types and Scopes
* User Controlled Eviction
* Change-list support calculated based on Create, Update and Inactivation Times 
    * {change_list, [{Add, Update, Delete}]} for a single Type/Scope; or, 
    * {change_list, [{Name_1, {A,U,D}},..., {Name_n, {A,U,D}}]} where Name = name 
      of Type or Scope for which the change_list refers
* Fine-grain logging via lager

###Components
* jc:                 Clusterable cache
* jc_http_server:     HTTP front-end to JCache offering get, post, put, delete
                      and long polling access to the cache
* change_relay:       Pub/Sub between JCache and other Erlang processes that 
                      want to be notified of cache changes. Used by jc_http_server
                      to support long polling, for example
* resource_discovery: Poor-man's, dynamic yellow-pages supporting clusteering and 
                      resource discovery that is 99% Martin Logan, Eric Merritt and
                      Richard Carlsson's as presented in Erlang and OTP in Action


### Gotchas
* Cache Values (as in Key, Value) must be printible unicode and are stored and returned 
  as binary printible unicode. As a convenience, Strings are accepted by the insert 
  functions, but those values are converted to binary before being persisted. Values are 
  returned as binary
* Keys, Types and Scopes can be any Erlang term, but if you want the results
  to be delivered as part of the jc_http_server's (JSON) response, all elements
  should by strings. Furthermore, the Value component MUST be valid JSON or it is not 
  guaranteed that the jc_http_server will accept it or return it
* Time-stamps are unix-style milliseconds since 1970, i.e. 1349285061414037
* Changing a Key's Type or Scope will be reflected in the "receiving" Type/Scope's 
  change list as an Add, but will not be reflected in the Update or Delete portion of the 
  "old" Type/Scope's change list
* Multiple JCache nodes on the same machine (ip address) require multiple configuration
  files so that the multiple jc_http_servers will all use different ports

### Clustering
* In Erlang, when A pings B and B pings C, all three nodes know about each other and 
can be thought of as being part of the same mesh (node discovery is transitive) 
* A JCache node pings a well-known Erlang node at start-up thereby joining the mesh
* It then registers with the resource_discovery service indicating that it offers the
JCache service and asks for other nodes offering JCache
  * If there are no additional nodes offering JCache -- or no other nodes in the mesh, 
    it creates the mnesia schema and starts as the only (first) mnesia instance 
  * If there are additional JCache nodes, it starts mnesia but joins the exsiting 
    schema / cluster 
* If the cluster involves nodes on different machines (different IP addresses) then they must
  be started with the same cookie 
 * erl -name yp@111.222.3.44 -setcookie somecookie; or,
 * erl -args_file vm.args,  where vm.args indicates the node name and cookie

###Configuration
* Configuration in a .config file -- typically sys.cofnfig
* mnesia section should not be adjusted
* sasl section defines the location of error logs, max bytes and max files for the logs which
  will be rotated
* lager
  * {lager_console_backend, level} where level can be one of debug, info, notice, warning, 
    error, critical, alert, or emergency
  * {lager_file_backend, [{file_path_and_name, level, maxSize, RotateTime, NoLogsToKeep}].
    One can have as many of the tuples as desired, so you can have one set of files for
    errors, one for debug, etc.
  * See  https://raw.github.com/basho/lager/master/README.org for more informatio
* jc
  * Contact_nodes defines the node(s) used as the well-known Erlang node used for clustering
    as described above. One node is sufficient, two is better.
  * table_wait_ms dictates how long mnesia is given to prepare tables for use and should be
    left alone
* resource_discovery
  * heart_beat_ms indicates how often the resource_discovery module checks the health of the
    mesh ensuring that nodes are stil up and responsive
* change_relay
  * evict_deadbeats_ms indicates how often the process sould look for clients that have no
    subscriptions and kick them out. There is no down side of this being on the order of 30
    minutes unless there is INSANE load
  * clean_buffer_ms is the time interval that the change_relay cleans-up temporary tables used
   to chunk updates into one update message. Shouldn't have to touch.
* jc_http_server has one configuration setting, the port number


###Starting a Release
* Releases include a configuration file and a boot file (at least). The boot file is a build
artificat that defines which applications are started with the node
* Machine 1
 * erl -name yp@111.222.3.44 -setcookie yummy
 * erl -name jc@111.222.3.44 -config config.sys -boot jc -setcookie yummy
* Machine 2
  * erl -name yp@222.222.3.44 -setcookie yummy
  * erl -name jc@222.222.3.44 -config config.sys -boot jc -setcookie yummy
* Notes
  * The Config file would indicate, in their jc section: {contact_nodes, ['yp@111.222.3.44', 'yp@222.222.3.44']}
  * a vm.args file could be used to indicate the cookie and node name in which case
    * erl -args_file vm.args -config sys.config -boot jc 


###Build Instructions
1. Ensure that Erlang 15B02 is installed
2. Get the Source Code and Dependencies
  * [root@dbo1] git clone git://github.com/jr0senblum/JCache.git
  * [root@dbo1] cd JCache
  * [root@dbo1] ./rebar get-deps
    * this will download mochiweb and place it in the deps directory
3. Compile Code
  * [root@dbo1] ./rebar clean
  * [root@dbo1] ./rebar compile
4. If rel/ is empty or suspect Make Release Node Specification, else goto 6.
  * [root@dbo1] cd rel
  * [root@dbo1] ../rebar create-node nodeid=jc -f
    * the -f forces the overwritting of whatever files might already be in the directory
5. Adjust configuration files
 * Copy ex_config/reltool.config to rel/reltool.config
   * Should be correct, but ensure that the following lines make sense
     * {lib_dirs, ["../lib", "../deps"]} <- paths to lib and deps directory,
     * {rel, "jc", 1, [kernel,...,jc,jc_http_server]} <- list should end with these last two
     * {app, jc, [{mod_cond, app}, {incl_cond, include}]} <- make sure app references jc
  * Copy ex_config/sys.config to rel/files/sys.config 
  * Copy ex_config/vm.config to rel/vm.config files
6. Adjust the IP address of rel/files/sys.config to reflect the machine being utilized
7. Adjust the IP address and cookie of rel/files/vm.config as appropriate
6. Generate the Release Node
  * [root@dbo1] change directorys back to JCache
  * [root@dbo1] ./rebar generate
  * The release will be in JCache/rel/jc
7. Run the release
  * [root@dbo1] ./bin/jc to see start-up options,
  * [root@dbo1] ./bin/jc foreground
  * [root@dbo1] ./bin/jc console
  * [root@dbo1] ./bin/jc ping
  * [root@dbo1] ./bin/jc/remote_console
8. The contents of the rel directory should be portable to any other machine with the identical
   architecture (CentOS 64 bit, for example) without needing to install Erlang on the target
   machine
