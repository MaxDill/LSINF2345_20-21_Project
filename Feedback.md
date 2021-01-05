# Comments

- your erlang sources are compatible **only** with erlang/otp >= 21, after some fixes your code runs smoothly (see my changes in sources).
- I appreciate the documentation, this helps to understand the logic you follow in your implementation with a brief explanation of every module
- the interesting part in this project was to verify your implementation analyzing the logs and plotting the in-degree. Concerning the logs, once I managed to make your source run, there is no way to get the view of nodes because during the execution the script `scenario1.sh` I cannot see an output logging the state of the nodes views. More importantly, arguing that you observe *the convergence of the view* (as you stated in the last section of your rapport) doesn't have any proof or base. Intuition is not enough in this project !

# Grade
| Bootstrap network (20%) | PS service implementation (50%) | Experimental scenario (30%) | Grade in % | Points (up to 5) |
|---|---|---|---|---|
|20	|35	| 0 |	55	|2.75|
