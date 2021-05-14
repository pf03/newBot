# BOT
*** 

## ORDER OF LAUNCH AND TESTING (tested only for Windows)

1.	Run commands:
   * git clone https://github.com/pf03/newBot.git
   * cd bot
   * stack build
3. To run the binary file successfully, copy the following files and folders from repository into the same folder with the binary file:
* config-example.json
3. Rename the config-example.json file to config.json and edit it

Be sure to edit the following fields:
* "apps.token"
* "apps.groupId" for VK

The fields "apps.repeatNumber" and "apps.updateId" can be left as they are, they will be filled in while the bot is running.
4. To test pure functions run ‘stack test’
***

## COMMANDS

/help - info about bot commands;
/start = /help;
/repeat - send keboard to edit repeat number for current user;
Buttons /1 ... /5 - edit repeat number for current user;
***

## MODULES

The bot operation logic is divided into the following layers (presented in the corresponding folders in the ‘src’) from low to high:
1. Common       - common functions;
2. Interface    - classes of types that implement abstract access of higher layers to interfaces:
  * MError  - error handling,
  * MLog    - logging,
  * MCache  - working with changing data in pure code,
  * Messenger - common interface for different messengers;
3. Logic    - the main logic of the program;
4. VK       - implementation of the messenger interface for API VK;
5. Telegram - implementation of the messenger interface for API Telegram;
6. T        - one of the possible implementations of the monadic interface - transformer T;
7. App      - application layer functions that have access to both the interface and its implementation.

Lower layers should not import modules from higher layers.