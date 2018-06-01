# conext
CLI tool to extract maven plugin / artifact timing data from jenkins consoleFull log.

## Get it

```bash
git clone git@github.com:jhrcek/conext.git
```

## Build it
This is Haskell project with stack configuration.
To build it you'll need to [install stack](https://docs.haskellstack.org/en/stable/README/#how-to-install).
After that you can build it using

```bash
cd conext
stack build --pedantic --test
```

## Run it
Download Full console log from which you'd like to extract build timing data.
```bash
wget https://kie-jenkins.rhev-ci-vms.eng.rdu2.redhat.com/job/kieAllBuild-master-jhrcek/13/consoleFull
```

Run the tool

```bash
stack exec conext-exe -- consoleFull
```

This will parse the log, extract data and produce output in 3 files in the current directory:
* artifact_durations.csv
* artifact_plugin_durations.csv
* plugin_durations.csv
