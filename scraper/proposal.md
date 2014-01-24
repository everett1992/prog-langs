###  Programming languages Ruby Project One
Caleb Everett

When you are logged into github.com you can see a list of your followers recent activity.
I'd like to summuraize that activity so I can quickly see what my followers are working on,
and how much they are using github.

 1. Implements a simple spider to scrape web pages for some specific data.

  I will use the [Nokogiri](http://nokogiri.org/) gem to parse the
  [following](https://github.com/everett1992/following) and
  [followers](https://github.com/everett1992/followers) pages to get the account names
  of all of a users followers, then I'll get the 
  [public activity](https://github.com/everett1992?tab=activity) of each user.

 2. Stores the data in a data structure (such as stack, tree, list, file, array).

    I'll define objects to represent a user, and different types of public activity,
    and I'll store these objects in arrays (or possibly account_id => Account hashes).

 3. Performs some simple manipulations on the data.

  I will summurize each follower, or followee's activity and list the repo's
  they are contributing to the most.

 4. Displays the stored data in some user-friendly format.

  The information will be output to stdout, with the most active followers first,
  and inactive followers ommited.
