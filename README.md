# CSE230_Final_Project_Action-Fighter

## Project Infomation

Project name: Action Fighter

Project members: Zhenyu Lu, Qunli Li, Bo Liu

Language: Haskell

Library: Brick

## Project Proposal

### Game Description

Action Fighter throws the game player into a fast-moving chaos as the player rides a motorbike through a bunch of tough enemies. Use the W, A, S, and D keys to move around—W goes up, A goes left, S goes down, and D goes right. The player should evade obstacles and control his bike precisely.

You don't have many lives, so get ready for a non-stop fight against the clock. When the time hits zero, your job is simple: take down as many enemies as possible to win. The player must navigate through various challenges, including avoiding crashes with curbs, enemy vehicles, enemy fire, and helicopter mines. A key feature is the “Haskell Truck”, which grants the player's vehicle extra capabilities upon successful docking.

Every time you face new challenges, be quick with your reactions and make smart choices. It is quite challenging to wipe out the enemies before you run out of lives. Get set to start your engine, dodge things in your way, and blast through the bad guys in Action Fighter—a thrilling test of speed, skill, and survival. This project aims to deliver a nostalgic yet fresh experience, highlighting the versatility and power of modern programming in game development.

### Implementation Strategy

- Haskell and Brick Library: Utilize Haskell's functional programming strengths and the Brick library for a robust text-based interface.
- Crash Mechanics: Implement various crash scenarios—contact with curbs, enemies, enemy fire
- Haskell Truck Interactions: Code the mechanics for docking with the Haskell Truck, acquiring power-ups, and managing the player's invincibility.


### Game Image
<img width="350" alt="e4e54eb9f1fd80ed8b848a2efdcc1db" src="https://github.com/zhenyucode/CSE230_Final_Project_Action-Fighter/assets/104359013/c7341681-4dae-48ce-93c7-37b9fcee71d9">


### Link to the online game demo
https://www.retrogames.cz/play_1533-SegaMS.php

## Milestone 2: Project Updates

### 1. Application Architecture

Our "Action Fighter" game, developed using Haskell and the Brick library, is structured around several key components:

- **Game Engine**: Manages the core gameplay mechanics, including vehicle transformations, enemy interactions, and Sega Truck power-ups.
- **User Interface (UI)**: Built with the Brick library, the UI handles all player inputs and displays game status, including vehicle type, collected parts, and remaining time.
- **Collision Detection System**: Responsible for implementing the game's crash mechanics, detecting collisions with curbs, enemy vehicles, and other obstacles.
- **Enemy AI Module**: Controls enemy vehicle behaviors and attack patterns, offering varied challenges to the player.
- **Timer and Score Manager**: Tracks the game timer and player's score, updating the UI accordingly.

### 2. Challenges and Solutions

So far, we have encountered a few challenges:

- **Complexity in Vehicle Transformation Logic**: The logic for vehicle transformation was initially complex. We simplified this by creating distinct state representations for each vehicle type, streamlining the transformation process.
- **Performance Optimization**: Ensuring smooth gameplay was challenging. We optimized performance by refining our collision detection algorithm and reducing unnecessary UI refreshes.

### 3. Project Goals and Deadline

- **Current Status**: We are on track with most of the project milestones. The foundational architecture is in place, and key game mechanics are functional.
- **Meeting the Deadline**: We are confident about meeting our goals by the deadline. The core gameplay elements are already implemented, and we are now focusing on refining the UI and enhancing the enemy AI.

### 4. Potential Modifications

- If unforeseen challenges arise, we may streamline some of the advanced features, such as reducing the complexity of enemy AI, to ensure the game is completed on time. However, our primary focus remains on delivering a complete and engaging game experience without compromising core functionalities.

### 5. Acknowledgment

In developing 'Action Fighter', our team got inspiration of how to use Brick library from a variety of existing projects, We extend our acknowledgment to the following projects for their influence:
[Dino-Brick](https://github.com/arcticmatt/dino-brick/tree/dino),
[Galaxian](https://github.com/ashybot/cse230-galaxian),
[2048Haskell](https://github.com/8Gitbrix/2048Haskell).
These projects provided us with valuable insights and served as a source of inspiration in terms of design ideas and coding approaches. However, it's important to note that 'Action Fighter' is an independent endeavor. Our game is the result of original thinking, innovative design, and dedicated programming by our team. We ensured that our creative process remained unique. We thank the creators of these projects for their contributions to the community, which indirectly supported our journey towards creating something new and exciting.

The following are distinctive features of our project:

- Innovative UI Design: In our 'Action Fighter' game, we've innovated with a road-like interface, confining both enemy and players within a road and enhancing visuals with roadside trees for a more engaging experience. We also solve the problem of lines not being straight by making sure all characters used for drawing the cells have same width.

- Two-Player Mode: Unlike many traditional arcade games, "Action Fighter" is designed for two players, promoting collaboration. Each player has individual controls, adding a layer of cooperative strategy to the game. Especially, when either player dies, the game will be over so that they can restart the game.

- Intuitive Control Scheme: The game employs a simple yet effective control system. Player 0 uses the 'WASD' keys for movement and the 'Space' key to shoot, while Player 1 navigates with the arrow keys and fires with the '.' key. This accessibility ensures a quick learning curve and immediate enjoyment.

- Interactive Leveling-Up Experience: In "Action Fighter", upon defeating all enemies, the game momentarily pauses. To advance to the next level, a player must actively choose to continue by pressing the 'y' key. This interactive mechanism not only makes players acutely aware of their progression but also puts them in control of the pace, adding an extra layer of engagement to the gaming experience.

- Smooth Game Flow with Score and Lives Carried Over: In 'Action Fighter,' the game keeps your score and lives as you move to the next level. This way, you can smoothly go from one level to the next, keeping your progress and planning your next moves. The game makes it easy to restart (press 'R') and quit when it's game over. Players decide when to level up, adding to the fun and challenge of the game.
