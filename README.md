# BOT
*** 

## ORDER OF LAUNCH AND TESTING (tested only for Windows)

1.	Run commands:
   * `git clone https://github.com/pf03/newBot.git`
   * `cd bot`
   * `stack build`
2. To run the binary file successfully, copy the following files and folders from repository into the same folder with the binary file:
* `config-example.json`
3. Rename the `config-example.json` file to `config.json` and edit it
Be sure to edit the following fields:
`apps.token`
* `apps.groupId` for VK

The fields `apps.repeatNumber` and `apps.updateId` can be left as they are, they will be filled in while the bot is running.

5. For the correct launch of the bot for VK, you must select `Work with API -> Long Poll API -> Event types -> Incoming message` 
in the community settings. The rest of the checkboxes shouldn't be checked.

![vk bot settings](https://raw.githubusercontent.com/pf03/newBot/main/vk_bot_settings.png)

6. To test pure functions run `stack test`
7. To exit the bot, type q+Enter
***

## CONFIG

Purpose of `config.json` fields:
* `app` - choice of VK or Telegram messenger;
* `defaultRepeatNumber` - default repeat number in response to the user's message (from 1 to 5);
* `log` - logging settings:
  * `colorEnable` - enable log colors in the terminal;
  * `fileEnable` - enable logging to the `log.txt` file;
  * `minLevel` - minimum level for logging turning on:
    * 0 - `Debug`
    * 1 - `Info`
    * 2 - `Warn`
    * 3 - `Error`
    * 4 - `Critical`
  * `terminalEnable` - enable logging to the terminal;
* `text` - template for responding to bot commands:
  * `help` - answer template to` / help` and `/ start` commands;
  * `repeat` - answer template to` / repeat` command;
  * `button` - answer template to` Buttons / 1 ... / 5` command;
  * `unknown` - answer template to unknown command;
* `apps` - a list of configs for different messengers, only the one specified in the` app` field is used:
  * `repeatNumber` - repeat number object, in which the key is the user id, the value is repeat number in response to the user's message (from 1 to 5);
  * `token` - a token for accessing the messenger API;
  * `updateId` - `updateId` value, that used if `updateIdFromFile` is` true`;
  * `updateIdFromFile` - enable reading updateId from` config.json` when starting the bot;
  * `name` - the name of the messenger (VK or Telegram);
  * `version` - API version (only for VK);
  * `groupId` - group id (only for VK);
  * `host` - API host.
***

## COMMANDS

* `/help` - info about bot commands;
* `/start` - the same as `/help`;
* `/repeat` - send keboard to edit repeat number for current user;
* `Buttons /1 ... /5` - edit repeat number for current user;
***

## MODULES

The bot operation logic is divided into the following layers (presented in the corresponding folders in the ‘src’) from low to high:
1. `Common`       - common functions;
2. `Interface`    - classes of types that implement abstract access of higher layers to interfaces:
  * `MError`  - error handling,
  * `MLog`    - logging,
  * `MCache`  - working with changing data in pure code,
  * `Messenger` - common interface for different messengers;
3. `Logic`    - the main logic of the program;
4. `VK`       - implementation of the messenger interface for API VK;
5. `Telegram` - implementation of the messenger interface for API Telegram;
6. `T`        - one of the possible implementations of the monadic interface - transformer T;
7. `App`      - application layer functions that have access to both the interface and its implementation.

Lower layers should not import modules from higher layers.