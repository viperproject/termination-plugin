# Termination Plugin - Viper

An extension/plugin for the [Viper verification infrastructure](http://www.pm.inf.ethz.ch/research/viper.html) that enables termination proofs.

### Compilation
From the termination-plugin subdirectory, create a symbolic link to the directory where you installed Silver:
```sh
$ ln -s ../silver silver
```

For testing also a symbolic link to Silicon is needed:
```sh
$ ln -s ../silicon silicon
```

### Usage
To use the plugin it has to be on the Java classpath. When invoking the back end `--plugin viper.plugin.termination.<plugin name>` has to be specified.

| Plugin Name | Tests |
| ------ | ------ |
| Decreases | Function with Path, Method |
| DecreasesFunction | Function |
| DecreasesFunctionPath | Function with Path |
| DecreasesMethod | Method |

### Test
You can run tests of the Termination Plugin by running `sbt test`.
