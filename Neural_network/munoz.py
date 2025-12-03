x = data[[col for col in ['season','yr', 'mnth', 'holiday', 'weekday','workingday','atemp', 'weathersit','temp', 'hum', 'windspeed']]]
#x = data[[col for col in ['atemp','temp', 'hum', 'windspeed']]] 
y = data.iloc[:,-1:]
y = (data.iloc[:, -1] > 4000).astype(int)


# Séparons le dataset en la partie train et test:

x_train, x_test, y_train, y_test = train_test_split(
    x, y, test_size=0.2, random_state=42, stratify = y
)

x_train = x_train.to_numpy()
x_test = x_test.to_numpy()

y_train = y_train.to_numpy()
y_test = y_test.to_numpy()

scaler = StandardScaler()
scaler.fit(x_train)

x_train = scaler.transform(x_train)
x_test  = scaler.transform(x_test)

x_train_tensor = torch.tensor(x_train, dtype=torch.float32)
y_train_tensor = torch.tensor(y_train, dtype=torch.long)

x_test_tensor = torch.tensor(x_test, dtype=torch.float32)
y_test_tensor = torch.tensor(y_test, dtype=torch.long)

train_dataset = TensorDataset(x_train_tensor, y_train_tensor)
test_dataset  = TensorDataset(x_test_tensor, y_test_tensor)

train_loader = DataLoader(train_dataset, batch_size=512, shuffle=True)
test_loader  = DataLoader(test_dataset, batch_size=512, shuffle=False)

num_features = x_train_tensor.shape[1]   
num_classes = 2                          

class MLP(nn.Module):
    def __init__(self):
        super().__init__()
        self.fc1 = nn.Linear(num_features, 16)
        self.fc2 = nn.Linear(16, 16)
        self.fc3 = nn.Linear(16, num_classes)
        self.relu = nn.ReLU()

    def forward(self, x):
        x = self.relu(self.fc1(x))
        x = self.relu(self.fc2(x))
        x = self.fc3(x)
        return x
device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
model = MLP().to(device)
model


criterion = nn.CrossEntropyLoss()
optimizer = torch.optim.Adam(model.parameters(), lr=0.001)


epochs = 200

val_acc = []

for epoch in range(epochs):

    model.train()
    running_loss = 0
    correct = 0
    total = 0

    for batch_x, batch_y in train_loader:

        batch_x, batch_y = batch_x.to(device), batch_y.to(device)

        optimizer.zero_grad()

        outputs = model(batch_x)             # logits
        loss = criterion(outputs, batch_y)   # batch_y = [0 ou 1]
        loss.backward()
        optimizer.step()

        running_loss += loss.item()

        _, predicted = torch.max(outputs, 1)
        total += batch_y.size(0)
        correct += (predicted == batch_y).sum().item()

    train_loss = running_loss / len(train_loader)
    train_acc = correct / total

    model.eval()
    val_loss = 0
    correct_val = 0
    total_val = 0
    

    with torch.no_grad():
        for batch_x, batch_y in test_loader:

            batch_x, batch_y = batch_x.to(device), batch_y.to(device)

            outputs = model(batch_x)
            loss = criterion(outputs, batch_y)
            val_loss += loss.item()

            _, predicted = torch.max(outputs, 1)
            total_val += batch_y.size(0)
            correct_val += (predicted == batch_y).sum().item()

    val_loss /= len(test_loader)
    val_acc.append(correct_val / total_val)

    # Affichage
    print(f"Epoch {epoch+1}/{epochs} | "
          f"Train Loss: {train_loss:.4f} | Train Acc: {train_acc*100:.2f}% | "
          f"Val Loss: {val_loss:.4f} | Val Acc: {val_acc[-1]*100:.2f}%")

print("Training complete!")


model.eval()

all_preds = []
all_labels = []

with torch.no_grad():
    for batch_x, batch_y in test_loader:
        batch_x = batch_x.to(device)
        outputs = model(batch_x)
        _, predicted = torch.max(outputs, 1)

        all_preds.extend(predicted.cpu().numpy())
        all_labels.extend(batch_y.cpu().numpy())


from sklearn.metrics import confusion_matrix, ConfusionMatrixDisplay

cm = confusion_matrix(all_labels, all_preds)
disp = ConfusionMatrixDisplay(confusion_matrix=cm, display_labels=[0, 1])
disp.plot()


plt.plot(val_acc, label="Val Acc")
plt.legend()
plt.title("Accuracy per epoch")
plt.show()



model.eval()

y_scores = []   # scores (probabilités/logits pour la classe 1)
y_true = []     # labels réels

with torch.no_grad():
    for batch_x, batch_y in test_loader:
        batch_x = batch_x.to(device)

        outputs = model(batch_x)  # logits shape: [batch, 2]

        # On récupère uniquement le score pour la classe positive (classe 1)
        scores = torch.softmax(outputs, dim=1)[:, 1]

        y_scores.extend(scores.cpu().numpy())
        y_true.extend(batch_y.cpu().numpy())

RocCurveDisplay.from_predictions(y_true, y_scores)