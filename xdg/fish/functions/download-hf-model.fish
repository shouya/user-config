# input a url like
# https://huggingface.co/TheBloke/Sakura-SOLAR-Instruct-GGUF/resolve/main/sakura-solar-instruct.Q4_K_S.gguf?download=true
# to download the llm model to the designated location

function download-hf-model --arg url
    set -l filename (echo $url | sed -e 's/.*\///' -e 's/\?.*//')
    er-x-direct-client add $(hostname -I)
    a2c -o $filename "$url"
    er-x-direct-client reset
end
