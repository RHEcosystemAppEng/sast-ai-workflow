# Deploying an Embedding Model on RHOAI

1. **Deploy MinIO and Create a Data Connection**  
   Set up MinIO on your OpenShift cluster and establish a data connection:  
   👉 [Deploy MinIO and Create Data Connection](https://ai-on-openshift.io/tools-and-applications/minio/minio/)

2. **Upload the Model to MinIO**  
   Download the model files from Hugging Face and upload them to your MinIO bucket:  
   👉 For example - [all-mpnet-base-v2 Model on Hugging Face](https://huggingface.co/sentence-transformers/all-mpnet-base-v2)

3. **Add the SBERT Runtime**  
   Integrate the SBERT (Sentence Transformer) runtime into your OpenShift AI environment:  
   👉 [SBERT Runtime Setup Guide](https://github.com/rh-aiservices-bu/llm-on-openshift/blob/main/serving-runtimes/sbert_runtime/README.md)

4. **Deploy the Model Using Single-Model Serving**  
   Utilize the single-model serving platform to deploy your embedding model with the configured data connection and SBERT runtime.

5. **Verify the Deployment**  
   Ensure the model is operational by sending a health check request:

   ```bash
   curl <your-external-endpoint>/health -H "Authorization: Bearer <your-token>"
   ```