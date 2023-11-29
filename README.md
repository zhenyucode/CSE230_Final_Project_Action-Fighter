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


