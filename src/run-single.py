import os
import textwrap
import glob

from Utils import read_source_code_file, extract_file_path
from MainProcess import MainProcess
from ReportReader import read_sast_report_html

NVIDIA_URL = "https://integrate.api.nvidia.com/v1"
NVIDIA_API_KEY = "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"
NVIDIA_LLM_MODEL_NAME = "meta/llama-3.2-3b-instruct"
NVIDIA_EMBEDDINGS_LLM_MODEL_NAME = "mistralai/mixtral-8x22b-instruct-v0.1"

git_repo_path = "https://github.com/redhat-plumbers/systemd-rhel10/tree/v256-15"
report_file_path = "/Users/jnirosha/Projects/morpheus/sast/systemd-252-46.el9_5.2.html"
html_file_path = "/Users/jnirosha/Projects/morpheus/sast/Confluence.html"

print(" Process started! ".center(80, '-'))
issue_list = read_sast_report_html(report_file_path)

main_process = MainProcess(base_url=NVIDIA_URL, llm_model_name=NVIDIA_LLM_MODEL_NAME,
                           embedding_llm_model_name=NVIDIA_EMBEDDINGS_LLM_MODEL_NAME, api_key=NVIDIA_API_KEY)



############ tmp code section START
issue_list = [i for i in issue_list if i.id == "def5"] ## CHANGE HERE!
# tmp_l = issue_list[0].trace.split("\n")
# selected_filename = tmp_l[1].split(".c")[0] if ".c" in tmp_l[1] else tmp_l[1].split(".h")[0]
all_lines = issue_list[0].trace.splitlines()
selected_filename = extract_file_path(all_lines[1])
############ tmp code section END

# downloading git repository for given project
# download_repo(git_repo_path)

# reading src folder and putting everything into embeddings
src_dir_path = os.path.join(os.getcwd(), "systemd-rhel10/src/")
doc_list = []
for filename in glob.iglob(src_dir_path + '/**/**', recursive=True):
    # only 1 .c file is embedding!
    if filename.endswith(".c") and os.path.isfile(filename):

        ######## tmp guard check
        if selected_filename in filename:
            for i in read_source_code_file(filename):
                doc_list.append(i.page_content)
            break
print(" Source code reading completed ".center(80))


# TODO consider iterating through all hyperlinks and extracting out the text
# contextual_html_data = read_html_file(html_file_path)



db = main_process.populate_db(doc_list)
print(" Vector db populated... ".center(80, '+'))


question = "Investigate if the following problem need to fix or can be considered false positive. " + issue_list[0].trace
response = main_process.query(db, question)
for l in textwrap.wrap(response, 80, break_long_words=False):
    print(l)
