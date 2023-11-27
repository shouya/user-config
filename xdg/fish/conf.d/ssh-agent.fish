# start ssh-agent
if status is-login; and status is-interactive; and not set -q SSH_AGENT_PID
    eval (ssh-agent -c)
end
