# input a url like
# https://huggingface.co/TheBloke/Sakura-SOLAR-Instruct-GGUF/resolve/main/sakura-solar-instruct.Q4_K_S.gguf?download=true
# to download the llm model to the designated location

function download-hf-model --arg url
    set -l filename (echo $url | sed -e 's/.*\///' -e 's/\?.*//')
    direct-dscp.sh aria2c -x10 -s10 -c --file-allocation=none -o $filename "$url"
end
