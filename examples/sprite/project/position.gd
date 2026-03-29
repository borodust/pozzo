extends Label


# Called when the node enters the scene tree for the first time.
func _ready() -> void:
	text = "Calculating position..."
	pass # Replace with function body.


# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(delta: float) -> void:
	pass

func _on_position_changed(new_pos: Vector2) -> void:
	text = "New Position: %v" % new_pos
