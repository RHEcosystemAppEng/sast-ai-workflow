from io import TextIOWrapper
import json
import logging

from common.config import Config
from pydantic import Field

from aiq.builder.builder import Builder
from aiq.builder.function_info import FunctionInfo
from aiq.cli.register_workflow import register_function
from aiq.data_models.function import FunctionBaseConfig

from dto.SASTWorkflowModels import SASTWorkflowTracker


logger = logging.getLogger(__name__)


class PreProcessConfig(FunctionBaseConfig, name="pre_process"):
    """
    Pre-processing function for SAST workflow.
    """
    description: str = Field(
        default="Pre-processing function that initializes the SAST workflow",
        description="Function description"
    )


@register_function(config_type=PreProcessConfig)
async def pre_process(
    config: PreProcessConfig, builder: Builder
):
    """Register the Pre_Process function."""
    
    logger.info("Initializing Pre_Process function...")
    
    def _temp_create_basic_sast_tracker_for_testing() -> SASTWorkflowTracker:
        """Just a temporary function for testing to create a SASTWorkflowTracker from a JSON file path"""
        logger.debug("Converting TextIOWrapper to SASTWorkflowTracker")
        try:        
            # Check if config field exists, if not create it
            if 'config' not in temp_data_for_testing:
                logger.info("Missing 'config' field in JSON, creating config object")
                temp_data_for_testing['config'] = Config()
            
            # Now create SASTWorkflowTracker with the complete data
            return SASTWorkflowTracker.model_validate(temp_data_for_testing)
        except Exception as e:
            logger.error(f"Failed to create SASTWorkflowTracker from temp_data_for_testing: {e}")
            raise


    
    async def _pre_process_fn(empty_input: dict) -> SASTWorkflowTracker:
        """
        Pre-processing function for SAST workflow.
        """
        logger.info("Running Pre_Process node - initializing workflow")
        
        # TODO: Implement actual pre-processing logic here
        
        # Create a basic SASTWorkflowTracker for the workflow
        tracker = _temp_create_basic_sast_tracker_for_testing()
        
        logger.info("Pre_Process node completed")
        return tracker

    try:
        yield FunctionInfo.create(
            single_fn=_pre_process_fn,
            description=config.description,
            input_schema=None  # No input for pre_process
        )
    except GeneratorExit:
        logger.info("Pre_Process function exited early!")
    finally:
        logger.info("Cleaning up Pre_Process function.") 
        
# This is a temporary data for testing the SASTWorkflowTracker, it will be removed when pre_process is implemented
temp_data_for_testing = {
  "max_iterations": 3,
  "iteration_count": 0,
  "issues": {
    "def1": {
      "original_report_data": "cockpit-310.2/src/bridge/cockpitdisksamples.c:220: freed_arg: \"free\" frees \"key\".\ncockpit-310.2/src/bridge/cockpitdisksamples.c:215: deref_arg: Calling \"strcmp\" dereferences freed pointer \"(char const *)key\".\n#  213|     while (fscanf (io_fp, \"%m[^: ]: %\" G_GUINT64_FORMAT \"\\n\", &key, &value) == 2)\n#  214|       {\n#  215|->       if (g_str_equal (key, \"read_bytes\"))\n#  216|           disk_read = value;\n#  217|         else if (g_str_equal (key, \"write_bytes\"))",
      "source_code": {
        "src/bridge/cockpitdisksamples.c": "def authenticate_user(user_id):\n    cursor.execute('SELECT * FROM users WHERE id = ' + user_id)\n    return cursor.fetchone()",
        "/app/models/user.py": "class User:\n    def __init__(self, id, username):\n        self.id = id\n        self.username = username"
      },
      "similar_known_issues": "Similar issue found in CVE-2019-1234: SQL injection in user authentication. Previous analysis showed parameterized queries needed. Status: FALSE POSITIVE after code review showed input validation present.",
      "analysis_response": None
    },
    "def2": {
      "original_report_data": "Cross-site Scripting (XSS) vulnerability found in profile template. CWE-79: Improper Neutralization of Input During Web Page Generation ('Cross-site Scripting'). Severity: MEDIUM. Location: /app/templates/profile.html:15",
      "source_code": {
        "/app/templates/profile.html": "<div class='profile-name'>\n    <h2>{{ user.name }}</h2>\n    <p>Welcome back, {{ user.display_name }}!</p>\n</div>",
        "/app/controllers/profile_controller.py": "def show_profile(user_id):\n    user = User.find(user_id)\n    return render_template('profile.html', user=user)"
      },
      "similar_knowqn_issues": "Similar XSS pattern found in previous scans. Template engines typically auto-escape by default. Need to verify if manual HTML insertion is happening. Previous similar issues were mostly FALSE POSITIVES due to auto-escaping.",
      "analysis_response": None
    }
  },
  "metrics": {}
}
