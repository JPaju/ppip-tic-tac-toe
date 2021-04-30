# Topics covered
* Getting started with a .NET Core F# project
* A bit about using Paket
* A bit about Suave
* Getting started with websockets and Suave
* Getting started with Elm websockets

![Silvrback blog image ](https://silvrback.s3.amazonaws.com/uploads/3a7ad300-9a9e-4106-82ae-9f48379ffee6/idevices-socket-product-photos-8.jpg)

# Why am I doing this?

I want to learn more about Elm, F#, Suave, websockets, and developing on .NET Core. So I decided to build a demo application with Elm on the front end, connecting to a Suave websocket server on the backend. I built this on .NET Core 2.

# Create a basic DotNet Core F# console project

To get started let's first scaffold out a basic .NET Core F# console project. In this example I am using .NET Core 2.0 SDK. You can install this on Windows, macOS, and Linux. I have it installed on both my [WSL with Ubuntu](https://docs.microsoft.com/en-us/windows/wsl/install-win10) and Windows 10.

Go [here](https://github.com/dotnet/core/blob/master/release-notes/download-archives/2.0.0-download.md) to learn more about how to install .NET Core for your system.

## Create a new project F# console project

.NET core provides a number of different templates to help you scaffold out a new project. I am going to create a console project in F#. Once you have the .NET Core 2.0 SDK installed you can do this. The command is the same for all OSes.

I will walk through setting up the project manually but as always you can find my code on Github:

Get the Suave websocket server [here](https://github.com/mandelbrotmesh/websocket-server).

Get the Elm websocket client [here](https://github.com/mandelbrotmesh/exosphere/tree/spike/elmwebsockets). Note this branch has not yet been merged into master.

First create a folder where you will keep your files. By default the name of the folder will be used to name your project files.

```bash
mkdir websocket-log
cd websocket-log
```

Now you can run the dotnet new command. I highly recommand running `dotnet new` to checkout all of the options available.

```bash
dotnet new console -lang F#
```

This creates `Program.fs`, which is the entry point for the console app, and a `websocket-log.fsproj` which configures your project and helps the compiler do its thing.

## Get Suave into the project

I am going to use Paket to manage dependencies. You can read more about it [here](https://fsprojects.github.io/Paket/paket-and-dotnet-cli.html). We can use Visual Studio Code to run Paket commands to do what you see in that link, so that is what I am going to do here.

Let's open our project in Visual Studio Code (VS Code) from the command line. Note: I did this from git-bash on my Windows rather than from the WSL because VS Code can't do intellisense if your dependencies are on the WSL and VS Code is running on Windows. I have .NET core installed on both WSL and Windows.

First make sure you are in your project directory.

```bash
$ ll
total 10
drwxr-xr-x 1 Marnee 197121    0 Dec 24 10:40 obj/
-rw-r--r-- 1 Marnee 197121  172 Dec 24 10:40 Program.fs
-rw-r--r-- 1 Marnee 197121 1809 Dec 24 10:47 README.md
-rw-r--r-- 1 Marnee 197121  252 Dec 24 10:40 websocket-log.fsproj
```

```bash
code .
```

This will open VS Code.

### Initialize paket

In VS Code hit Ctl + Shift + P. This opens a command window. Enter paket and you should see a number of command options. Select `Paket: Init`. 

![Silvrback blog image ](https://silvrback.s3.amazonaws.com/uploads/70f3a37c-3fbf-4516-9862-83fb0d0fdfbd/vscodepaket.png)

This creates a number of new files and folders related to paket. You will put a list of your dependencies in `paket.dependenies` and this is where we will put our reference to Suave.

Open `paket.dependencies`.

Notice that there isn't much in there except:

`source https://www.nuget.org/api/v2`

This means that paket will use nuget as a source of packages.

Below this line add a reference to Suave.

`nuget suave`

Your file will look like this

```text
source https://www.nuget.org/api/v2
nuget suave
```

Now we want paket to bring Suave and its dependencies into our project. We can use VS Code to do this.

Hit Ctl + Shift + P and enter `Paket` and then select `Paket: Install`.

Note: VS Code will display a log of everythihng Paket is doing in the terminal window under OUTPUT.

Once that is done you can open the `packages` folder and see all of the dependencies paket pulled in.

Now we need to add Suave to the project using the .NET Core CLI. Make sure you are in the same directory as your project file and do this:

```bash
dotnet add package suave
```

This will add Suave to the project file.

# A bit about Suave

> Suave is a simple web development F# library providing a lightweight web server and a set of combinators to manipulate route flow and task composition.

This means you can build a web server and even a REST API with Suave by composing small parts together. You can learn a little more on my previous article on [upgrading the Suave bootstrapper](https://marnee.silvrback.com/suave-bootstrapper-upgraded).

# Building a websocket server in Suave

![Silvrback blog image ](https://silvrback.s3.amazonaws.com/uploads/d14390f4-2bbb-4623-9d5c-510633dd2e65/suave.gif)

To do this I followed [this example in the Suave Github repo](https://github.com/SuaveIO/suave/tree/master/examples/WebSocket). Basically I copied the code from the `Program.fs` file into my project's `Program.fs` file.

Note: after copying in the code I got a lot or red-squigglies. To get rid of them and get some intellisense, you'll want to build the project. Go to the command line, make sure you are in the same directory at the project file and then

```bash
dotnet build
```

Note: If you work in a Dropbox folder you will need to pause syncing to get `dotnet build` to run, otherwise you will probably get permission errors like this.

>C:\Program Files\dotnet\sdk\2.1.2\Microsoft.Common.targets(127,3): error MSB4024: The imported project file "C:\Users\Marnee\Dropbox\github\project-mandelbrot\websocket-log\obj\websocket-log.fsproj.proj-info.targets" could not be loaded. The process cannot access the file 'C:\Users\Marnee\Dropbox\github\project-mandelbrot\websocket-log\obj\websocket-log.fsproj.proj-info.targets' because it is being used by another process. [C:\Users\Marnee\Dropbox\github\project-mandelbrot\websocket-log\websocket-log.fsproj]

## Some of the example code doesn't build

I am still working on why this is happening, but the example code doesn't build because of one line (see line 84):

```fsharp
path "/websocketWithSubprotocol" >=> handShakeWithSubprotocol (chooseSubprotocol "test") ws
```

This is the error:

 > error FS0039: The value or constructor 'handShakeWithSubprotocol' is not defined. Maybe you want one of the following:↔   handShakeResponse↔   handShake [C:\Users\Marnee\Dropbox\github\project-mandelbrot\websocket-log\websocket-log.fsproj]

 I have looked in the Suave source code and `handShakeWithSubprotocol` is in the code.

 ![Silvrback blog image ](https://silvrback.s3.amazonaws.com/uploads/1c003eab-df54-4f48-990a-eefd849ded10/fc%2C800x800%2Cwhite.u2.jpg)

 I just commented out this line and was able to get the code to `dotnet build`.

 Now you have a Suave webserver with support for a websocket.

 You can run it like this:

 ```bash

 dotnet run

```

 This starts a webserver that you can connect to with:

 `http://localhost:8000`

 ```bash
 $ dotnet run
[12:12:13 VRB] Creating buffer bank, total 827392 bytes
[12:12:13 DBG] Initialising BufferManager with 827392
[12:12:13 INF] Smooth! Suave listener started in 125.597 with binding 127.0.0.1:8080
```

W00t! You now have a websocket server. Now let's try to use it.

### It's a simple chat

The Suave websocket example sets up a websocket server that can act as a simple chat. It just echoes back what is sent to it. Let's walk through the code.

At the bottom of Program.fs we have `main`, the entry point of the console app. Here we start our webserver with verbose logging.

```fsharp

[<EntryPoint>]
let main _ =
  startWebServer { defaultConfig with logger = Targets.create Verbose [||] } app
  0

```

Moving up we see `app`, which is a `WebPart`, [the basic building block of a Suave webserver](https://suave.io/composing.html).

```fsharp

let app : WebPart = 
  choose [
    path "/websocket" >=> handShake ws
    // path "/websocketWithSubprotocol" >=> handShakeWithSubprotocol (chooseSubprotocol "test") ws
    path "/websocketWithError" >=> handShake wsWithErrorHandling
    GET >=> choose [ path "/" >=> file "index.html"; browseHome ]
    NOT_FOUND "Found no handlers." ]

```

`choose` gives us the option to define routes. Here we have two paths that are combined with functions that will execute if we go to that route. `handShake` is a combinator that

> captures a Websocket and passes it to the provided continuation

#### What's a continuation?

> [A continuation is a function that you pass into another function to tell it what to do next.](https://fsharpforfunandprofit.com/posts/computation-expressions-continuations/)

The `/websocket` is a websocket handling function that will execute `ws`.

The `/websocketWithError` is a websocket handling function that will execute `wsWithErrorHandling`.

`ws` is a function that takes a `WebSocket` and an `HttpContext` and sets up a loop that continuously reads from the websocket and sends a response. `wsWithErrorHandling` does the same with a bit of error handling.

# Elm websockets

Lucky for us, the [Elm websocket example](https://guide.elm-lang.org/architecture/effects/web_sockets.html) assumes a websocket server echos back what is sent to it, so I didn't have to change the Suave code to make this work.

If you want to learn more about getting started with Elm, see my pervious article on using Elm and IPFS, [You can't stop the signal](http://marnee.silvrback.com/you-can-t-stop-the-signal). You can get the code from my [Github repo](https://github.com/mandelbrotmesh/exosphere/tree/spike/elmwebsockets). Note this branch has not yet been merged into master.

The code from the Elm websocket example will work with the Suave websocket server we created with only one modification. In the source code, look for `suave-connection.elm`.

Find `echoServer`. Here is where we tell the client the URL to the websocket server, in this case I changed the example code to `ws://localhost:8080/websocket`, which is my Suave websocket server.

To run the Elm client do the following:

```bash

elm-make suave-connection.elm

```

This creates the `index.html` file.

Now you can run the Elm client with:

```bash

elm-reactor

```

This starts a webserver that you can get to with `localhost:8000`.

```bash

$ elm-reactor
elm-reactor 0.18.0
Listening on http://localhost:8000

```

Start the Suave websocket server. Remember we do it like this.

```bash

 dotnet run

```

Now we can try out the Elm client. Load it up in your browser:

`http://localhost:8000`

In the text box enter something and click `Send`. You should get some text back. If you check your browsers developer console, on the Network tab you should see the WS transaction.

![Silvrback blog image ](https://silvrback.s3.amazonaws.com/uploads/d7026d97-14f2-497a-a83c-04d8a9500f57/success-kid.jpg)

# What's next?

Ok this is cute and all but I should probably try building something a bit more useful. I am thinking about a websocket client that displays a continuous log. Also I really need to find out what is going on with `handshakewithsubprotocol`.

Update: [I answered my own question on StackOverflow](https://stackoverflow.com/questions/47965104/the-value-or-constructor-handshakewithsubprotocol-is-not-defined-in-websocket/47965105#47965105). Looks like this function is not a part of v2.2.1, which is the latest stable release. 

As always if you have any questions please tweet me.
