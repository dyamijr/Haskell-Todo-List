# Haskell-Todo-List
## 1 Goal 
This project aims to build a simple command-line interface (CLI) todo list application. This project will explore Haskell’s core features, such as functional programming, state management, file I/O, and handling user input/output. In this project, users can add, remove, list, and mark tasks as done in a to-do list. Additionally, users can merge/combine lists, and save lists to continue working between sessions. This project should help maintain the organization of daily tasks and display and manage them efficiently. Each task has a description, due date, priority, and category. The lists can be sorted by due date, priority, or description and filtered by category or completion status.

## 2 Use Cases

1. One use case of the program is to plan daily tasks. It allows the user to add tasks or items they need to complete and mark them as completed once done. This is useful for everyday tasks such as grocery shopping where they could add the items they need to buy to the list to remember them.
2. Another use case is assignment and test planning for students. Students sometimes struggle to manage the work they need to complete, especially with the variety of deadlines they might have. This program would let them add any homework or exams they have, as well as set a due date/time. This allows them to sort the list to show the items with the closest deadlines first, making it easy to prioritize assignments that are due soon.
3. The program could also be used as a simple project management tool. In a project that’s broken into tasks, the tasks can be added to the to-do list to make them easier to manage. This also lets you see the clear milestones you meet along the way to completing a project and plan the timetable using the dates. If the project has multiple main areas, the tasks can have categories to make it easier to filter for those specific areas.
   
## 3 Initial design

1. Task Module: All items on the to-do list can be broken down into tasks. Tasks are the core functionality and maintain each task on the list. They are responsible for storing a description, a completed state, the priority of the task, a potential due date or deadline, and recurrence. This module will be responsible for CRUD (Create-Read-Update-Destroy) operations for tasks as well as management for all other task elements.
2. To-do List Module: This module is responsible for maintaining and managing the to-do list. The to-do list will be stored as a list of tasks and this module has the functionality of adding, removing, and sorting of task items.
3. File System Module: This module will handle all tasks relating to the file system. Users will have the option to save and load a list to use in between sessions. This module will pull saved lists from the file system and parse them back into use for our to-do list module as well as write and save the list in the current directory.
4. Parsing Module: This module is responsible for handling user input and determining the task to complete based on the user input. Some of the basic inputs will be add, remove, complete, list (by completed, uncompleted, or all), sort, and quit session. This module is responsible for parsing the user input and determining the task that should be completed.
5. User Interface (UI) Module: The purpose of this module is to format the list in a clean easy-to-view display for the user. Everything that is displayed to the user will be done in this module.
6. Main Module: This is the central module that will handle and call each module when necessary. This module will be the heartbeat of the project.

## 4 Testing

The main areas of functionality that should be tested are adding/removing tasks, marking tasks as complete, saving/loading lists, merging/combining lists, sorting by date and title, and filtering by category. Testing these areas will be done manually by performing the actions and verifying the proper behavior. A list of test cases will be made for each feature, consisting of actions and expected behavior. Additionally, we plan to perform User Acceptance Testing (UAT) using a small group of participants. We will have them test the core functionality listed above and provide scores on a scale from 1-5 for each test case/function. All users should be able to easily operate and manage the system with very little input from developers.

To run the unit tests run the following. 
```
cabal update
cabal build --enable-tests
cabal test
```

## 5 How to Run

To run the project you will use the .cabal file for the project. 
```
cabal update
cabal build
cabal run TodoApp
```


