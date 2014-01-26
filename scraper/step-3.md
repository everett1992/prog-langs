1. What is the programming paradigm (model) for Ruby? Provide a brief rationale for your answer.

  Ruby is a object oriented, imperative, and functional language. Every variable is an Object, there
are no primitives. Even `Integer`, and `String` are objects. Methods of objects are usually called
one after the other as in the imperative paradigms. Ruby's `Block`'s, `Proc`'s, `Method`'s, and `lambda`'s
are used throughout Ruby programs.

2. What typing system does Ruby use? Provide a brief rationale for your answer.

  Ruby is a dynamically typed, using what is offend styled 'duck typing'. If an `Object` walks like
a duck and talks like a duck then it is a duck. Variables are not assigned types on instantiation,
and an `Object`s type is never checked automatically by the interpreter.

3. What are the major strengths of Ruby? Provide a brief explanation for each strength listed.

  Ruby is a language that is vary readable and can express a lot if a few method calls. This makes
it especially good for prototyping, and useful for projects that don't need to be performant.
Many programs will benefit from the readability and expressiveness of Ruby, more than they will
suffer from it's speed.

4. What are the major weaknesses of Ruby? Provide a brief explanation for each weakness listed.

  Ruby's lack of speed makes it unpopular for many applications. People I've talked to haven't tried
to learn Ruby because they felt learning a faster language would be more beneficial.
I think Ruby lacks Pythons mind-share among beginner programmers, which causes Ruby's community to be small.

5. For what types of applications is Ruby most suitable? Provide a brief explanation for each type of application.

  Ruby is great for small scripts, like the ones I use to download podcasts, check on backups, and add convenience
functions to my OS. The Ruby on Rails environment makes Ruby very good for web development, Rails is the best web
framework I've used.

6. Does Ruby handle concurrency? Explain why or why not.


  Some argue that the default ruby implementation (mri) doesn't support parallelism because of the Global Interpreter Lock(GIL).
Non Ruby parts of the interpreter are not thread safe, so they use locks to prevent data corruption.
Other Ruby implementations like JRuby, Rubinius, and  MacRuby don't use GIL's and feature true parallelism.
Ruby does include concurrent and parallel constructs like `Thread`s, and `Fiber`s and their use can lead to
like-concurrent performance benefits.

http://merbist.com/2011/02/22/concurrency-in-ruby-explained/


