how to compile and load:
c(name) in the shell
compile:file/1
code:load_file/1
code:is_loaded/1
code:is_loaded/1
code:purge/1
code:soft_purge/1

purging old code:
When a new version is loaded, that version becomes the current and the code it replaces becomes the old. And the previous old code is purged, any module that's running that version of the code will be terminated.

We load some new code. The client module still uses the code it was running until there's a call in that module any function in Foo. And at that point, when that function call is executed, the module starts using the new version of Foo for every possible function.

All the functions in Foo, it switches over to use the new version like so. So there we see it carries on using the version it was using until it reaches a function call.

When a module calls itself recursively and we load a new version, when there is a fully qualified call to a function in Foo, it switches. But if there are only non-qualified calls, the switch is not triggered. So if, for example, you want to write a loop where you expect to update the body of the loop, you need to make sure the recursive call to the loop is a fully qualified call.

Then what happens to modules using the old code? What happens is that they'll carry on using the old code, and that code is terminated, the code is purged, and the module will be terminated. So if you're using the old code, that module will be terminated.

Call code:purge or code:soft_purge to trigger a purge. If you call soft_purge, it removes an old version of the code only if it isn't being used. Purge will always succeed, but it may cause things to fail.

cheat sheet:
I think I finally understand this. Here are the keys: 
1. Any function that recurses forever (specifically loop in this example) MUST be fully qualified when called in BOTH old and new code. This enables the hot loaded new code to be used on the recursive call. Note, you don't have to fully qualify the call to loop in init since it doesn't recurse. Note, you DO have to export loop/0. 
2. Call code:soft_purge/1 to ensure than an existing old version of the code is removed before load. 
3. Load the new code with code:file_load/1. It doesn't matter what directory you are in. You can overwrite the old beam with a new one and this will work fine. 
4. Do NOT try to use any new features in the recursed function (loop) until you have recursed one more time as the old version is still running. For example, if you add inject/1 and implement it by adding another clause to loop/0, your new clause will NOT be in the current running version of loop/0 even after file_load/1 and soft_purge/1 UNTIL you do something that results in recursion thereby replaces the old loop with the new. Try calling allocate/0 first to force a recurse and start using the new code. Now you can call inject/1 and it will work. (edited)

