"""
This script opens a new issue on the
https://gitlab.adacore-it.com/eng/libadalang/langkit-query-language/ repository
about checking and removing all deprecated interfaces that has been deprecated
for at least one release.
"""

from datetime import datetime
from e3.auth.gitlab import gen_gitlab_token
import gitlab

# Username of the Gitlab user the new issue is going to be assigned to
DEFAULT_ASSIGNEE = "guerrier"

if __name__ == "__main__":
    # Create a connexion to Gitlab and fetch the LKQL project (id #277)
    gl = gitlab.Gitlab(
        url="https://gitlab.adacore-it.com",
        private_token=gen_gitlab_token()["token"],
    )
    project = gl.projects.get(277)
    assert project.path == "langkit-query-language", "Fetched project isn't LKQL"

    # Get the user that the new issue should be assigned to
    users = gl.users.list(username=DEFAULT_ASSIGNEE, get_all=False)
    assert len(users) == 1, "Cannot fetch the user to assign the issue to"

    # Get the current and the previous release names
    current_release = str(datetime.now().year)[2:]
    previous_release = str(int(current_release) - 1)

    # Create the object defining the new issue
    new_issue = {
        "title": f"Remove deprecated interfaces for release {current_release}",
        "description": f"""
Please review and remove all LKQL interfaces that have been deprecated for the
{previous_release} release.

Look for LKQL interfaces annotated with `@deprecated` and consider their removal if
this annotation is present since the {previous_release}.0 version at least.""",
        "labels": ["Area::Language"],
        "assignee_ids": [users[0].id],
    }

    # Finally, create the issue using the Gitlab API
    project.issues.create(new_issue)
    print("Issue created!")
