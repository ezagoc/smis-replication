import tensorflow as tf
import tensorflow_hub as hub
import tensorflow_text as text
from official.nlp import optimization
tf.get_logger().setLevel('ERROR')

from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Input, Embedding, Dense, Dropout

# Importing the model from tensorflow

tfhub_handle_encoder = "https://tfhub.dev/tensorflow/small_bert/bert_en_uncased_L-4_H-512_A-8/1"
tfhub_handle_preprocess = "https://tfhub.dev/tensorflow/bert_en_uncased_preprocess/3"

# Build Neural Network with BERT

#text_input = tf.keras.layers.Input(shape=(), dtype=tf.string, name='text') #input
text_inputs = [tf.keras.layers.Input(shape=(), dtype=tf.string)]

#preprocessor = hub.KerasLayer(tfhub_handle_preprocess, name='preprocessing') #BERT tokenizer
#tokenized = preprocessor(text_input)
preprocessor = hub.load(tfhub_handle_preprocess)

tokenize = hub.KerasLayer(preprocessor.tokenize)
tokenized_inputs = [tokenize(segment) for segment in text_inputs]

seq_length = 256  # Your choice here.

bert_pack_inputs = hub.KerasLayer(
    preprocessor.bert_pack_inputs,
    arguments=dict(seq_length=seq_length))  # Optional argument.

encoder_inputs = bert_pack_inputs(tokenized_inputs)

encoder = hub.KerasLayer(tfhub_handle_encoder, trainable=True, name='BERT_encoder') #BERT embedding and encoding
embedded = encoder(encoder_inputs)

net = embedded['pooled_output']
#net = tf.keras.layers.Dropout(0.1)(net)
#net = tf.keras.layers.Dense(32, activation='relu',)(net)
net = tf.keras.layers.Dropout(0.1)(net)
net = tf.keras.layers.Dense(1, activation=None, name='classifier')(net)

# Defined model architecture:
model_BERT = tf.keras.Model(text_inputs, net)

#### Function to predict certain text in a data frame: 


def predictions_bert2(df_import, column = "text"):
    model_path = '../../africacheck/models/cp2.cpkt'
    model_BERT.load_weights(model_path)
    df = clean_data(df_import, column)
    sentences = list(df['text_clean'])
    text = np.array([str.encode(preprocess_text(sen)) for sen in sentences], dtype=object)
    predictions = model_BERT.predict(text).flatten()
    y_pred_nn = (predictions > 0.5).astype(np.int32)
    
    df_import['true'] = y_pred_nn.tolist()
    df_import.drop(['text_clean'], axis=1, inplace=True)
    
    return df_import