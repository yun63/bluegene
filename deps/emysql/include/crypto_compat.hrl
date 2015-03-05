%% Note: This file was automatically generated. Do not include it in source control
-define(HASH_SHA(Data), crypto:sha(Data)).
-define(HASH_FINAL(Data), crypto:sha_final(Data)).
-define(HASH_UPDATE(Data, Salt), crypto:sha_update(Data, Salt)).
-define(HASH_INIT(), crypto:sha_init()).
