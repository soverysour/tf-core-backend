# Trade Forall

## Summary

A solution that allows creation of trading platforms - similar to e-commerce, however, trading is done among the users of the platform, e.g.:

- Small community that revolves around collectibles (card games, figurines, etc)
- Game items / currency trading platform (similar to poe.trade, trade.tf, etc)
- Purely virtual platform that (possibly) integrates with other services (i.e. a platform that allows trading of github issues, requests for work, favour, etc)
- Cross-platform trading solution (trading items for any other item, not in the same game).

## Components

- **core**: Frontend component - responsible for handling custom layouts, themes, other platform metadata, consuming the API.
- **core**: Backend component - *main* backend component - responsible for handling the main events that can occur (post item, change price, etc).
- **ergonomic**: UI platform creator - component providing a pleasant front for the "Parse config" component - for non-technical users.
- **extra**: Analytics - component responsible for generating reports, based on platform activity.
- **extra**: User profile pages, for reviews, comments, etc.
- **dependency**: Integrator - component responsible for orchestrating foreign API / platform integrations.
  - **extra**: Steam Integration - steam integration, would allow reading steam inventory information, etc.
  - **extra**: Github Integration - github integration, would allow treating issues / projects as items, etc.
- **extra**: Bots - component that would allow running of bots and automatization of tasks.
- **extra**: Alarms - triggering alarms when certain market conditions are met.
- **extra**: Browser plugin - provide a proper API and allow plugin creators to use it.
- **non-functional**: Scalability - scalable services, application scaling to handle any load of users.
- **non-functional**: Correctness - don't bother with security / bugs; all that is handled.
- **non-functional**: Isolation - use docker to run all components (backend, frontend, database).
