# Contributing to `crvsreportpackage`

Thank you for your interest in contributing to `crvsreportpackage`!
We welcome contributions from everyone. Here are some guidelines to help you get
started.

## Reporting Issues

If you encounter any bugs or have suggestions for improvements, please create
an issue on GitHub. When reporting an issue, please provide as much detail as
possible, including:

- A clear and descriptive title.
- A detailed description of the issue.
- A rough topic that the issue relates to.
- Steps to reproduce the issue if a bug, alongside relevant error messages.
- Acceptance Criteria for what you would consider a satisfactory solution.

## Submitting Contributions

### Make sure the repository is set up

If you are external to the development team, please fork the repository.
If you are part of the development team, make sure you have cloned the repo
successfully.

## Create a new branch

Create a new branch off the `development` branch.
Make sure that the name is relevant to the issue you are trying to solve.

## Make Your Changes

1. Make your changes to the codebase, in line with the existing style.
2. Write or update tests as appropriate.
3. Ensure that your code works on the test data as desired.
4. Document any new functionality or changes in the appropriate documentation.

## Test your Changes

Make sure that any changes you have introduced do not have knock-on effects on
the rest of the codebase by running the full test suite:

```bash
devtools::test()
```

## Commit Your Changes

Commit your changes with a descriptive message:

```bash
git add .
fit commit -m "Feat: Description of the changes"
```

When making commits to any branch, we follow the
[DSC Coding Standards](https://datasciencecampus.github.io/coding-standards/version-control.html#git-style-guide)
guidelines on version control.
This means every commit should aspire to have a message following the structure:

```git
<type>: <subject>
<BLANK LINE>
<body>
<BLANK LINE>
<footer>
```

Where the `<type>` element is one of the following tags:

- feat: A new feature
- fix: A bug fix
- doc: Documentation only changes
- style: Changes that do not affect the meaning of the code.
    For example, white-space, formatting, missing semi-colons, etc.
- refactor: A code change that neither fixes a bug or adds a feature
- perf: A code change that improves performance
- test: Adding missing tests
- chore: Changes to the build process or auxiliary tools and libraries.
    For example, documentation generation.

The `<subject>` element should finish the sentence:

_If accepted this commit will <subject>_.

The `<body>` element should be a short description that provides info on what
files were changed and a bit more detail on the changes made.

Finally, the `<footer>` can contain auxiliary info, such as the ticket this
commit is working towards fixing.

## Push Your Changes

Push your changes to the relevant branch:

```bash
git push origin my-feature-branch
```

## Submit a Peer Review

1. Go to the repository on GitHub and navigate to the "Pull Requests" tab.
2. Click on the "New Pull Request" button.
3. Select your feature branch, where you have made your changes.
4. Select the `development` branch as the destination.
5. Provide a clear and descriptive title for your pull request.
6. Provide a detailed description of your changes (link to an existing issue).
7. Submit the pull request.

## Peer Review Process

Once a new pull request is created, it will be subject to a peer review. 
The review process should follow the guidelines set out in the
[duck book](https://best-practice-and-impact.github.io/qa-of-code-guidance/peer_review.html).
Reviewers should follow a series of steps:

- Read and understand the ticket and its acceptance criteria.
- Have a quick look through the GitHub pull request and see what has changed.
- Update their data and code locally to ensure they are testing in the right
environment. Make sure you are on the same branch.
- Run the test suite and ensure all the code conforms to the style guide.
- Run the pipeline and ensure any functionality claims have been achieved.

Throughout this process reviewers should write comments, using the GitHub
functionality is usually a good starting place. All changes should be requested
and communication between peers is strongly encouraged. In order to standardise
the review process reviewers should follow the template bellow. This will ensure
consistent feedback:

```
##  Code review

#### Documentation

Any new code includes all the following forms of documentation:

- [ ] **Function Documentation** as docstrings within the function definition.
- [ ] **Examples** demonstrating major functionality, which runs successfully locally.

#### Functionality

- [ ] **Installation**: Installation or build of the code succeeds.
- [ ] **Functionality**: Any functional claims of new code have been confirmed.
- [ ] **Automated tests**: Unit tests cover essential functions for a reasonable range
  of inputs and conditions. All tests pass on your local machine.
- [ ] **Packaging guidelines**: New code conforms to the project contribution
  guidelines.
---

### Review comments

*Insert detailed comments here!*

These might include, but not exclusively:

- bugs that need fixing (does it work as expected? and does it work with other code that it is likely to interact with?)
- alternative methods (could it be written more efficiently or with more clarity?)
- documentation improvements (does the documentation reflect how the code actually works?)
- additional tests that should be implemented (do the tests effectively assure that it  works correctly?)
- code style improvements (could the code be written more clearly?)

Your suggestions should be tailored to the code that you are reviewing.
Be critical and clear, but not mean. Ask questions and set actions.
```

Once the merge has been approved, the ticked should be closed and the relevant
branch deleted. If there is any tech-debt associated with this ticket it must
be spun-off into new tickets before the merge happens.

## Getting Help

If you need help or have any questions, feel free to reach out by opening an
issue or contacting the maintainers directly.

Thank you for contributing!
