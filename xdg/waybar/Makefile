update: config
	pkill -USR2 waybar

config: config.yaml
	yaml2json <$< | jq . >$@
