# BOT
*** 

## ORDER OF LAUNCH AND TESTING

1.	Run commands:
   * `git clone https://github.com/pf03/newBot.git`
   * `cd bot`
   * `stack build`
2. To run the binary file successfully, copy the following files and folders from repository into the same folder with the binary file:
* `config-example.json`
3. Rename the `config-example.json` file to `config.json` and edit it
Be sure to edit the following fields:
* `apps.token`
* `apps.groupId` for VK

The fields `apps.repeatNumber` and `apps.updateId` can be left as they are, they will be filled in while the bot is running.

4. For the correct launch of the bot for VK, you must select `Work with API -> Long Poll API -> Event types -> Incoming message` 
in the community settings. The rest of the checkboxes shouldn't be checked.

![vk bot settings](https://raw.githubusercontent.com/pf03/newBot/main/vk_bot_settings.png)

5. To test pure functions run `stack test`
***

## CONFIG

Purpose of `config.json` fields:
* `name` - bot name, which must be in list of `apps.name` fields;
* `forks` = true - run all bots, that specified in `apps` (`apps.enable` must be true) in parallel mode; `forks` = false - run only one bot, that specified in `name` field
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
  * `app` - the name of the messenger (VK or Telegram);
  * `name` - bot name, must be unique;
  * `enable` - enable current bot in parallel mode;
  * `repeatNumber` - repeat number object, in which the key is the user id, the value is repeat number in response to the user's message (from 1 to 5);
  * `token` - a token for accessing the messenger API;
  * `updateId` - `updateId` value, may be `null`;
  * `version` - API version (only for VK), bot tested only with `version=5.50`;
  * `groupId` - group id (only for VK);
  * `host` - API host.
***

## FUNCTIONAL
The main function of an echo bot is to send the same message to the user in response to a message a specified number of times. The bot also supports some commands (see the list below). Two messengers are supported - Telegram, VK.  

Message types for the Telegram messenger:
* text;
* sticker;
* gif;
* photo;
* video;
* doc;
* poll;
* contact;
* location;
* forward;
* other message types should also be supported, but this is not guaranteed  

Message types for the VK messenger:
* text
* sticker;
* photo;
* video;
* doc;
* audio;
* wall;
* link  
If a message with an attachment is not sent by the community owner, then such attachments will be ignored (the `access_key` for such attachments must be obtained separately using the `getHistoryAttachment` method, which is not implemented in this bot)
***

## COMMANDS

* `/help` - info about bot commands;
* `/start` - the same as `/help`;
* `/repeat` - send keyboard to edit repeat number for current user;
* `Buttons /1 ... /5` - edit repeat number for current user;
***