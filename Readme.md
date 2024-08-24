# tstools

This is an R package I created to support my undergraduate economic forecasting and graduate time series econometrics classes. The goal was to improve the user experience to the point that students could learn the methods without getting sidetracked with hacking over strange design decisions, but without turning everything into a black box. At this point, the functionality is also useful for research.

The documentation is incomplete (to be generous). You may need to look at the source code to figure out what's going on.

# What happened to the Bitbucket repo?

Interesting that you ask. For more than a decade, this package was publicly available in a Bitbucket repo. The short story is that Bitbucket gave ownership of all my repos (public and private) to someone else without telling me. After sufficient prodding, and with much reluctance, they finally told me what they had done. After many weeks, I regained access to my repos. 

Then in 2024, someone (hard to say who) removed my access to my public and private repos again and I never regained access. I moved the most recent version of the package to this Github repo. The plan is to keep it here permanently.

It won't surprise you to learn that I recommend against trusting Atlassian with critical data. It was shocking to lose access to my repos, but it was more shocking that 

- Atlassian didn't care and tried to hide it from me
- they didn't want to fix it
- they didn't have any concerns about things violations of FERPA or NDAs. 

You should do a thorough risk assessment if you use Atlassian products in a business.

# Installation

```
library(devtools)
install_github("bachmeil/tstools")
```

# Warning

In general, there shouldn't be code breakage, but I'm not making any promises. I'm happy to break compatibility when there's a good reason to do so.
