y_train = LabelEncoder().fit_transform(train["Topic"])
y_val = LabelEncoder().fit_transform(test["Topic"])
y_train = to_categorical(y_train)
y_val = to_categorical(y_val)