# twilio
Functions for accessing the twilio API from within R

# Installing
Generate a personal access token: https://github.com/settings/tokens

You need the 'repo' access levels assigned.

```R
install.packages("remotes")
remotes::install_github(
  repo = 'workingamerica/twilio',
  auth_token = '' # personal access token here, between quote marks
)

library(twilio)
```

## Permanently save twilio credentials as environment variables:

Run this:

```R
usethis::edit_r_environ()
```

It'll open a file called `.Renviron`, which is saved in your home folder. (On Windows, this is of the form `C:/Users/myusername/Documents/.Renviron`.) Enter environment variables like so (no quotes), save, close, and restart R.

```
TWILIO_ACCOUNT_SID = ACxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
TWILIO_AUTH_TOKEN = xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
```

Now you can access environment variables using `Sys.getenv()`, e.g.:

```R
tw.incoming_phone_numbers.list(
	sid = Sys.getenv('TWILIO_ACCOUNT_SID'), 
	token = Sys.getenv('TWILIO_AUTH_TOKEN'), 
	n = 20
)
```

Keep in mind the secrets are saved in plaintext on your computer, so this isn't great for non-shared credentials.