This repo originates from this article (link to NDC article) and the associated talk
at NDC London 2014 (link to slideshare).

Conway's Game of Life cell automaton is used to show how to think like an Erlanger.

The principle of using asynchronous message passing and thinking in terms of
protocols describing the interactions between processes is applicable to a number of
settings, not just Erlang, but the overlay of using Erlang supervision is much easier
and straightforward to do in Erlang.

The contents of the repo has moved on from the initial version used for the article
and the talk at NDC - even the talk code was improved from the article's - but I have
tried to tag relevant revisions to make it easier for you to cherry pick a particular
stage in the evolution of the code.

The wiki for this repo contains a more in-depth explanation of the thinking process
and the tools used than what you will find here with the code.

Notable tags:
* ndc1 - tag for the NDC article's code.

Notable branches:
* permanent_collector - instead of spawning a new collector for each time step, the
  collector process is re-used. It saves so little resources (5%) that it is not
  worth the hassle to do it.
  
