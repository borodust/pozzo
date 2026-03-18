@tool
extends EditorScript

func _run() -> void:
	var extensions = GDExtensionManager.get_loaded_extensions()
	for extension in extensions:
		print("Extension: ", extension)
	var	obj = HelloGodot.new()
	print("Object:", obj)
	print("Result:", obj.ping())
	print("Level Before:", obj.level)
	obj.level = 3
	print("Level After:", obj.level)
	print("Exec Path:", OS.get_executable_path())
	print("Cmd Line:", OS.get_cmdline_args())
	print("User Cmd Line:", OS.get_cmdline_user_args())
