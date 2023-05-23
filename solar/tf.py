import numpy as np
import tensorflow as tf
import cv2


def tensorflow_tflite(img_path, model):
    image = cv2.imread(img_path, cv2.IMREAD_UNCHANGED)

    interpreter = tf.lite.Interpreter(model_path=model)
    interpreter.allocate_tensors()

    input_details = interpreter.get_input_details()
    output_details = interpreter.get_output_details()

    image_rgb_224px = cv2.resize(cv2.cvtColor(image, cv2.COLOR_BGR2RGB), (224, 224), interpolation=cv2.INTER_AREA)

    input_data = np.asarray(image_rgb_224px, dtype=np.float32)

    interpreter.set_tensor(input_details[0]['index'], [input_data])
    interpreter.invoke()
    output_data = interpreter.get_tensor(output_details[0]['index'])

    return output_data[0, :, :, 1]
