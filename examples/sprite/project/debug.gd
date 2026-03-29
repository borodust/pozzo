@tool
extends EditorScript

func _run() -> void:
	var extensions = GDExtensionManager.get_loaded_extensions()
	for extension in extensions:
		print("Extension: ", extension)
	print("Exec Path:", OS.get_executable_path())
	print("Cmd Line:", OS.get_cmdline_args())
	print("User Cmd Line:", OS.get_cmdline_user_args())

	var	obj = HelloGodot.new()
	print("Object:", obj)
	print("Amplitude Before:", obj.amplitude)
	obj.amplitude = 25.0
	print("Amplitude After:", obj.amplitude)
