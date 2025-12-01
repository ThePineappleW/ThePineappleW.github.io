---
title: The Programmer's Guide to Hosting a Minecraft Server
---

The simplest way to self-host a Minecraft server is to go to [minecraft.net](https://www.minecraft.net/en-us/download/server), download `minecraft_server.1.*.jar` and run it on your computer. This works great... until you want to use third-party server software, manage multiple servers on one computer, or track incremental changes to server files over time.

These challenges are ubiquitous for pretty much all software that gets deployed on a server. Thankfully, we have a rich ecosystem of tools to deal with them. I want to discuss how I've used tools like Docker for Minecraft.

Please keep in mind that my intended use case (a Java-edition server with <10 players) may be different from yours. If you have any advice or suggestions, please feel free to reach out to me.

## Docker

This writeup is not intended to serve as an introduction to [Docker](https://www.docker.com), so if you haven't used it before I suggest you follow some tutorials before using it for Minecraft. The key thing to understand is that a Docker container is a great way to manage a Minecraft server.

This task is made easy by the [`docker-minecraft-server`](https://github.com/itzg/docker-minecraft-server) image by `itzg`. There's also a version for Bedrock edition, but I have no experience with that. I like to use Docker Compose to specify my containers, since it makes version control incredibly simple. All you need to do it create a file called `compose.yaml` and specify your configuration options. Here's the template I use:

```yaml
services:
  mc:
    image: itzg/minecraft-server
    tty: true
    stdin_open: true
    ports:
      - "25565:25565"
    restart: "always"
    environment:
      EULA: "TRUE"
      MEMORY: "6144M"
      TZ: "America/New_York"
      VERSION: "1.21.8"
      DIFFICULTY: "2"
      SPAWN_PROTECTION: "0"
      OPS: |-
        <your-player-name>

    volumes:
      - ./data:/data
```

Here's a quick explanation of some of the settings:

- `tty` and `stdin_open`: Settings for input and output through the terminal. Don't change these unless you know what you are doing.

- `ports`: A range of ports exposed by the container. `25565` is the default port for Minecraft, so again, I wouldn't change this.

  - If you have something that requires extra ports, like a server map or voice-chat mod, you will need to add those ports here.
  - **DO NOT** add the RCON port here, unless you want people to be able to access your server console from outside networks.

- `restart`: Specifies the restart policy of the server. The value `always` means that the server will automatically be started again after it is stopped. Thus, you can restart the server by simply invoking the `/stop` command.

- The `environment` settings correspond to Minecraft server settings, and should be self explanatory. You don't actually have to provide a game version, since the Docker image will use the latest by default. However, I don't like it when my server updates without my explicit permission, so I always specify a game version.

  - See the [documentation](https://docker-minecraft-server.readthedocs.io/en/latest/configuration/server-properties/) for a full list of `server.properties` settings that can be specified here.

- `volumes`: A mapping from directories on your computer to virtual directories in the Docker container. This can be a little tricky if you have't used Docker before. In my compose file, I state that the `data` directory (Make sure to create this directory in the same parent directory as `compose.yaml`) should be used to house all of the server files. This directory is then mounted to the container as a volume called `/data`.

**I highly recommend** this [configurator tool from setupmc.com](https://setupmc.com/java-server/). It gives you a GUI for generating your compose file, and has explanations for many of the settings.

> Note: Docker requires root access by default. I recommend adding your user to the Docker group so you don't have to `sudo` the following commands.

Once you've set up your compose file, starting your server is simple. Make sure you navigate to the directory containing `compose.yaml`. Then run the following command, and you should see an output that looks similar.

```bash
$ docker compose up -d
[+] Running 2/2
 ✔ Network server_default  Created       0.1s
 ✔ Container server-mc-1   Started       2.0s
```

The `-d` flag starts the container detached. To attach the container, and access the server console, you can run:

```bash
$ docker compose attach <container-name>
```

`<container-name>` is the string shown when you created your container, e.g. `server-mc-1` in the example above.  
This command should plop you into the container terminal. To detach the container again, press `ctrl-p` then `ctrl-q`.

As discussed above, stopping the server _application_ will just cause it to restart again. To terminate the entire Docker container, run the following command:

```bash
$ docker compose down
```

When the server is down, it is safe to modify the files in the `data` directory. Doing this while the server is running can cause files to become corrupted and should be avoided.

I like to put these three commands into short shell scripts for convenience, so that I can just run `./start.sh` etc., instead of remembering the Docker command syntax each time.

The overall structure of the server directory should look something like this:

```
server
├── data
│   └── <server files>
├── docker-compose.yaml
├── attach.sh
├── start.sh
└── stop.sh
```

Another benefit of Docker is that it handles shutdown commands gracefully. For example, if you wanted to reboot your system, you would want to make sure that the Minecraft server saves the world and stops itself before terminating, or else your data could become corrupted. Docker will automatically propagate the shutdown command, eliminating the headache.

While Docker can become incredibly complex for larger projects, it makes running a Minecraft server incredibly simple.

## Modded Servers

I rarely run vanilla Minecraft servers nowadays. Even when I make a server which does not require any client-side mods, I usually use [server-side Fabric mods for optimization](https://modrinth.com/mods?e=server&f=categories:optimization) and other utilities. The traditional way to manage server mods involves manually downloading `.jar` files, placing them into the `mods` directory, and praying. If anything needs to be updated or has a version conflict (you might be familiar with the pain of installing large custom modpacks), it can take hours to find appropriate files and revert to a stable version.

Surely there is a better way to do this.

### Packwiz

[Packwiz](https://packwiz.infra.link) is a tool that serves as, essentially, a package manager for Minecraft mods. It provides a nice command-line interface for installing and updating mods, and specifying mod versions in plaintext. Installing it makes mod management much easier, and as a bonus, it integrates well with our existing Dockerized approach.

After you've installed packwiz, make a new directoy in your server directory. I like to call mine simply `packwiz`. Inside that directory, you can run `packwiz --help` for the full list of commands.

First, initialize your modpack with `packwiz init`. You will be prompted for information about your pack, which you should fill in with the appropriate details. You can also just hit enter for default values.

```bash
$ packwiz init
Modpack name [Modpack]: server-pack
Author: Emery
Version [1.0.0]:
Minecraft version [1.21.10]: 1.21.8
Mod loader [quilt]: fabric
Fabric loader version [0.18.1]:
index.toml created!
Refreshing index... 0 % [------------------------------------------] done
pack.toml created!
```

This creates two files:

- `pack.toml`: Specifies the metadata you provided about your pack.
- `index.toml`: Will contain a list of mods you to install. That list be empty for now.

The nice thing about packwiz is that you never have to open these files yourself. The CLI commands will edit them for you.

Now, we can add our mods. There are a bunch of ways to do this, so I recommend looking at the packwiz documentation and finding the methods that work best for you. I prefer to use [Modrinth](https://modrinth.com), and add mods by slug.

> A _slug_ is the useful bit in a mod's Modrinth URL.  
> For example, in `https://modrinth.com/mod/ferrite-core`, the mod slug is `ferrite-core`.

Let's install three mods for now:

- Ferrite Core
- Lithium
- Krypton

For each one, run `packwiz modrinth install`:

```bash
$ packwiz modrinth install ferrite-core
Project "FerriteCore" successfully added!

$ packwiz modrinth install lithium
Project "Lithium" successfully added!

$ packwiz modrinth install krypton
Project "Krypton" successfully added!
```

This process will automatically install mod dependencies, and will fail if the given mod doesn't support the version of our pack.

Note two things. First, the `index.toml` file now contains references to all of the installed mods. Second, we have a `mods` directory containing `.pw.toml` files each specifying the url and version info of a specific mod. Again, you don't have to touch any of these files directly, but it's good to understand what they represent.

The `packwiz` directory should look like this:

```
packwiz
├── index.toml
├── mods
│   ├── ferrite-core.pw.toml
│   ├── krypton.pw.toml
│   └── lithium.pw.toml
└── pack.toml
```

Keep in mind that we have _not_ actually download any of the mods yet. Packwiz has only created files that tell us _which_ mods to download, and from _where_.

Now it's time to integrate our modpack with the server. `docker-minecraft-server` is especially great because it supports modded servers right out of the box. There are only a few updates we need to make to the compose file.

There are two `environment` fields to add:

- `TYPE: "Fabric"` is self-explanatory.
  - You can also specify the Fabric version via `FABRIC_LAUNCHER_VERSION: <...>`. Omitting this will default to the latest Fabric release for your server Minecraft version.
- `PACKWIZ_URL: "/packwiz/pack.toml"` tells the Docker image where to find the modpack to install. The combination of Packwiz and Docker means that you can specify and install a modpack without ever downloading files manually, which is awesome.

Note that the value for `PACKWIZ_URL` does **not** refer to the `packwiz` directory in our server directory. Instead, it refers to a virtual directory in the Docker container. Just as we had to explicitly tell Docker that the `data` directory should be mounted to the container as `/data/`, we have to do the same for `packwiz`. Therefore, we need a new item under `volumes`:

- `./packwiz:/packwiz`

This step is crucial, and your modpack will not work if you forget to do this!

Now, the updated `compose.yaml` will look like this:

```yaml
services:
  mc:
    image: itzg/minecraft-server
    tty: true
    stdin_open: true
    ports:
      - "25565:25565"
    restart: "always"
    environment:
      EULA: "TRUE"
      TYPE: "Fabric"
      MEMORY: "6144M"
      TZ: "America/New_York"
      VERSION: "1.21.8"
      DIFFICULTY: "2"
      SPAWN_PROTECTION: "0"
      OPS: |-
        <your-player-name>
      PACKWIZ_URL: "/packwiz/pack.toml"

    volumes:
      - ./data:/data
      - ./packwiz:/packwiz
```

And the file structure of the entire server will look like this

```
├── data
│   └── <server-files>
├── docker-compose.yaml
├── packwiz
│   ├── index.toml
│   ├── mods
│   │   └── <modpack-files>
│   └── pack.toml
├── attach.sh
├── start.sh
└── stop.sh
```

You should now be able to start your server, and `docker-minecraft-server` will automatically grab your mods.

## Git

One of the many benefits of using Docker Compose and packwiz is that the server files can be generated deterministically from a set of text files. And the best part of working exclusively with text files is that they can be tracked seamlessly with Git.

In fact, I have a single Git repo containing all of my servers. It has proven useful for version control, as I have the ability to revert to earlier versions in the frequent case that I mess up my modpacks. Another benefit is portability: setting up a server on a different machine is as simple as pulling from GitHub.

`docker-minecraft-server` is very powerful, and can supply pretty much all of the configuration items you would normally specify in `server.properties`.

The one major caveat is that you should not attempt to track your server files themselves with Git, since the data files can be many gigabytes in size. As such, you should blacklist the data directory in your `.gitignore`:

```
**/data/
```

**Do not use Git for world backups**.

If you want to back up your worlds, you should create copies of the `data` directory locally or cloud machine, or preferably, use one of the many server-backup mods/plugins/tools that exist on the Internet.

## Scripting

One of the nice things about running a server on your own machine is that you can interact with it programmatically. Minecraft servers support a protocol called `RCON`, which lets you remotely execute commands. We can use this protocol in external scripts for fun and profit.

### `RCON`

> Note that `RCON` is insecure, as it transmits your password via plaintext, and gives an authenticated user a high degree of control over your server.
>
> `docker-minecraft-server` sets a random `RCON` password by default, and so you should only use the provided `rcon-cli` interface.
>
> Again, you should **NEVER** expose the `RCON` port (25575).

You can execute commands through the `RCON` interface as so:

```bash
$ docker exec <container-name> rcon-cli <command> ...
```

Here are some examples:

```bash
$ docker exec clms-mc-1 rcon-cli list
There are 0 of a max of 20 players online:

$ docker exec clms-mc-1 rcon-cli say Hello World!
# No output, but players in-game will see `[rcon] Hello World!`
```

### Server Restart Script

How might we take advantage of this functionality?

One common task is automating server restarts. It's good to restart a server frequently to avoid memory leaks and other conditions that can cause lag. The best time to restart a server is in the middle of the night, since the fewest players will be online at 2AM. However, it's not fun to wake up every night to manually restart the server. And if there _are_ players online, they should be given a few minutes to wrap up before being kicked off.

Because we set the Docker restart policy to `always`, we just have to safely stop the server, and Docker will start it up again.

We can use the `RCON` interface to write a script that will automatically stop the server after a given delay. We'll use the `subprocess` module to execute shell commands.

First, we check if there are players online. If there are none, we can shut down right away. If anybody is online, we issue a warning. Then, we wait for a customizable delay. We give another warning at the 1-minute mark. And finally, we shut the server down.

Here's the full script, with a nice CLI. You can omit the `argparse` bit in favor of `sys.argv`, if you are OK with a more spartan interface.

```python
# restart.py
#!/usr/bin/env python3

import argparse
from subprocess import run
import time


def rcon(cmd: str) -> str:
    """Tries to run a command through the server's RCON interface."""
    args = cmd.split()
    output = run([*args], capture_output=True).stdout
    return str(output)


def online_players(server_name: str) -> int:
    """Get the number of online players, or -1 if there is an error."""
    try:
        player_listing = rcon(f"docker exec {server_name} rcon-cli list")
        return int(player_listing.split()[2])

    except Exception:
        return -1


def warn_server(server_name: str, delay_s: int) -> None:
    """Warn the server about an impending reboot."""
    minutes, seconds = divmod(delay_s, 60)

    if minutes:
        delay_time = f"{minutes} minute(s)"
        if seconds:
            delay_time = f"{delay_time} and {seconds} second(s)"
        else:
            delay_time = "5 seconds"
    else:
        if seconds:
            delay_time = f"{seconds} second(s)"
        else:
            delay_time = "5 seconds"
    rcon(
        f"docker exec {server_name} rcon-cli say Server will automatically reboot in {delay_time}."
    )


def warn_and_wait(server_name: str, delay_s: int) -> None:
    """Warn players, then wait the appropriate duration."""
    if delay_s <= 60:
        # If the delay is a minute or less, only warn once.
        warn_server(server_name, delay_s)
        time.sleep(delay_s)

    else:
        # Otherwise, warn again at the 1 minute mark.
        warn_server(server_name, delay_s)
        time.sleep(delay_s - 60)
        warn_server(server_name, 60)
        time.sleep(60)


def shut_down(server_name: str) -> None:
    """Shuts down the server."""
    rcon(f"docker exec {server_name} rcon-cli say Rebooting server...")
    time.sleep(5)  # Add a slight delay just in case anybody is still online.
    rcon(f"docker exec {server_name} rcon-cli stop")


def main(server_name: str, delay_s: int) -> None:
    num_online = online_players(server_name)
    if num_online == -1:
        return  # If the server isn't running or there is some other error, do nothing.

    if num_online > 0:
        warn_and_wait(server_name, delay_s)
    shut_down(server_name)


if __name__ == "__main__":
    ap = argparse.ArgumentParser(
        prog="Minecraft Server Auto-Restarter",
        description="A script that shuts down a Minecraft server in a Docker container after a delay.",
    )
    ap.add_argument(
        "-n", "--name", type=str, required=True, help="The name of the Docker container"
    )

    ap.add_argument(
        "-d",
        "--delay",
        type=int,
        required=False,
        default=300,
        help="The delay in restarting the server in seconds after the initial announcement. Do not make this more than 59m59s.",
    )
    args = ap.parse_args()
    main(args.name, args.delay)
```

Now, we just have to make the script executable, and it should work.

```bash
chmod +x restart.py
```

### Cron

The last piece of the puzzle is to automate the running of our script. You could theoretically use Python or the language of your choice to automate this, but there exists a simpler alternative that comes preinstalled with most Linux distros: Cron.

If you are not familiar with Cron, I recommend you go through a [tutorial](https://linuxhandbook.com/crontab/#difference-between-cron-crontab-and-cron-job) before using it for the first time, as I won't discuss all the intricacies here.

To automate our script, we need to add a new job to the Cron table. Let's say we want to run our script nightly at 2AM. First, open your crontab file:

```bash
crontab -e
```

This should open a text editor (perhaps after prompting your for which one to use).  
Add the following line:

```
0 2 * * * ~/path/to/restart.py
```

Then close the editor. The five positions before the path to the script specify the time it should be run.  
From the left, the script will be run on:

- `0`: the 0th minute
- `2`: of the 2nd hour
- `*`: of every day of the month
- `*`: of every month of the year
- `*`: on every day of the week.

Note that your system clock might not be set to your timezone. Mine is set to GMT, but I live in GMT-5:00, so I would set my hour digit to `7`.

Save the file and then your script will be set to run! When you take your server down, you can run `crontab -e` again and comment out this line.

You can use cron for any number of tasks, such as automating backups.

## Conclusion

These are some of the tools I use in maintaining Minecraft servers. Keep in mind that people keep developing new ones, so it's a good idea to periodically check forums to see what people are using. Furthermore, there are a lot of ways to run a Minecraft server, and different approaches might be suited to each one.

Overall, the important takeaways from this writeup are:

- You can use developer tools to great effect in hosting Minecraft servers.

- Things that are good practice in software development, such as containerization and dependency management, are useful elsewhere.

- Be careful not to expose your Minecraft server to external threats. Use a whitelist. Keep online mode enabled. And do not expose any more ports than you absolutely need.
  - I am not responsible if you get pwned.
