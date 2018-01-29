using Newtonsoft.Json.Linq;
using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net.Sockets;
using System.Threading;
using UnityEngine;
using UnityEngine.UI;

public class CrossroadController : MonoBehaviour {
    //Prefabs
    [SerializeField]
    private GameObject pedestrian;
    [SerializeField]
    private GameObject car;
    [SerializeField]
    private GameObject carTurningLeft;

    //UI
    [SerializeField]
    private Button connectButton;
    [SerializeField]
    private Button exitButton;
    [SerializeField]
    private Button generateButton;
    [SerializeField]
    private InputField pedestriansInputField;
    [SerializeField]
    private InputField carsInputField;
    [SerializeField]
    private GameObject pedestrians;
    [SerializeField]
    private GameObject cars;
    [SerializeField]
    private GameObject generateButtonGO;

    //Materials
    [SerializeField]
    private Material red_light;
    [SerializeField]
    private Material yellow_light;
    [SerializeField]
    private Material green_light;
    [SerializeField]
    private Material no_light;

    //Connection
    static TcpClient client = null;
    static BinaryReader reader = null;
    static BinaryWriter writer = null;
    static Thread networkThread = null;
    private static Queue<Message> messageQueue = new Queue<Message>();

    //
    List<String> fasterTurns = new List<string>();
    void Awake()
    {
        DontDestroyOnLoad(this);
    }

	// Use this for initialization
	void Start () {
        connectButton.onClick.AddListener(CreateConnection);
        exitButton.onClick.AddListener(CloseApp);
        //generateButton.onClick.AddListener(GeneratePedestriansAndCars);

    }

    private void CloseApp()
    {
        if(networkThread != null)
        {
            networkThread.Abort();
            client.Close();
            reader.Close();
            writer.Close();
        }
        Application.Quit();
    }

    // Update is called once per frame
    void FixedUpdate()
    {
        processMessage();
    }

    private void CreateConnection()
    {
        StartServer();
        GameObject.Find("ConnectButton").SetActive(false);
        //pedestrians.SetActive(true);
        //cars.SetActive(true);
        //generateButtonGO.SetActive(true);
    }

    // TODO
    private void GeneratePedestriansAndCars()
    {
        int pedestriansToCreate = int.Parse(pedestriansInputField.text);
        int carsToCreate = int.Parse(carsInputField.text);
        //String generate = "lol";
        byte[] lol = new byte[] { 70, 80 };
        Send(new Message(lol));
    }


    static void AddItemToQueue(Message item)
    {
        lock (messageQueue)
        {
            messageQueue.Enqueue(item);
        }
    }

    static Message GetItemFromQueue()
    {
        lock (messageQueue)
        {
            if (messageQueue.Count > 0)
            {
                return messageQueue.Dequeue();
            }
            else
                return null;
        }
    }

    private void processMessage()
    {
        Message msg = GetItemFromQueue();
        if(msg != null)
        {
            
            String myJson = System.Text.Encoding.Default.GetString(msg.Content);
            //Debug.Log(myJson);
            JObject json = JObject.Parse(myJson);

            String action = json["action"].ToString();
            String pid;
            GameObject go;
            switch (action)
            {
                
                case "pedestrian_spawned":
                    pid = json["pid"].ToString();
                    go = GameObject.Instantiate(pedestrian);
                    go.name = pid;
                    float x = float.Parse(json["position_x"].ToString()) * 2;
                    float z = float.Parse(json["position_y"].ToString()) * 2;
                    go.transform.position = new Vector3(x, 0.5f, z);
                    break;
                case "pedestrian_disappeared":
                    pid = json["pid"].ToString();
                    go = GameObject.Find(pid);
                    GameObject.Destroy(go);
                    break;
                case "pedestrian_move":
                    pid = json["pid"].ToString();
                    go = GameObject.Find(pid);
                    x = float.Parse(json["position_x"].ToString()) * 2;
                    z = float.Parse(json["position_y"].ToString()) * 2;
                    float speed = int.Parse(json["speed"].ToString())/1000;
                    StartCoroutine(MovePedestrian(go, new Vector3(x, 0.5f, z),speed));
                    break;
                case "car_spawned":
                    pid = json["pid"].ToString();
                    String turn = json["turn"].ToString();
                    if (turn.Equals("left"))
                    {
                        go = GameObject.Instantiate(carTurningLeft);
                    }
                    else
                    {
                        go = GameObject.Instantiate(car);
                    }
                    go.name = pid;
                    x = float.Parse(json["position_x"].ToString()) * 2;
                    z = float.Parse(json["position_y"].ToString()) * 2;
                    if (z == 16f)
                        go.transform.Rotate(0, 270, 0);
                    else if (x == 14f)
                        go.transform.Rotate(0, 180, 0);
                    else if (z == 14f)
                        go.transform.Rotate(0, 90, 0);
                    go.transform.position = new Vector3(x, 0.375f, z);

                    turn = json["turn"].ToString();
                    StartCoroutine(StartIndicator(go,turn));
                    break;
                case "car_disappeared":
                    pid = json["pid"].ToString();
                    go = GameObject.Find(pid);
                    fasterTurns.Remove(pid);
                    GameObject.Destroy(go);
                    break;
                case "car_move":
                    pid = json["pid"].ToString();
                    go = GameObject.Find(pid);
                    speed = int.Parse(json["speed"].ToString()) / 1000;
                    x = float.Parse(json["position_x"].ToString()) * 2;
                    z = float.Parse(json["position_y"].ToString()) * 2;
                    StartCoroutine(MoveCar(go, new Vector3(x, 0.375f, z),speed));
                    break;
                case "car_turn_left":
                    pid = json["pid"].ToString();
                    go = GameObject.Find(pid);
                    StartCoroutine(RotateCar(go, "left"));
                    break;
                case "car_turn_right":
                    pid = json["pid"].ToString();
                    go = GameObject.Find(pid);
                    StartCoroutine(RotateCar(go, "right"));
                    break;
                case "car_faster_turn_left":
                    pid = json["pid"].ToString();
                    fasterTurns.Add(pid);
                    break;
                case "lights_changes_to_green":
                    ChangeLights("green");
                    break;
                case "lights_changes_to_yellow":
                    ChangeLights("yellow");
                    break;
                case "lights_changes_to_red":
                    ChangeLights("red");
                    break;
            }
        }
    }

    private IEnumerator StartIndicator(GameObject go,string which)
    {
        switch (which)
        {
            case "left":
                while(go != null)
                {
                    go.transform.Find("Body/LeftIndicators/FrontIndicator").GetComponent<MeshRenderer>().material = yellow_light;
                    go.transform.Find("Body/LeftIndicators/SideIndicator").GetComponent<MeshRenderer>().material = yellow_light;
                    go.transform.Find("Body/LeftIndicators/BackIndicator").GetComponent<MeshRenderer>().material = yellow_light;
                    yield return new WaitForSeconds(0.33f);
                    go.transform.Find("Body/LeftIndicators/FrontIndicator").GetComponent<MeshRenderer>().material = no_light;
                    go.transform.Find("Body/LeftIndicators/SideIndicator").GetComponent<MeshRenderer>().material = no_light;
                    go.transform.Find("Body/LeftIndicators/BackIndicator").GetComponent<MeshRenderer>().material = no_light;
                    yield return new WaitForSeconds(0.33f);
                }
                break;
            case "right":
                while (go != null)
                {
                    go.transform.Find("Body/RightIndicators/FrontIndicator").GetComponent<MeshRenderer>().material = yellow_light;
                    go.transform.Find("Body/RightIndicators/SideIndicator").GetComponent<MeshRenderer>().material = yellow_light;
                    go.transform.Find("Body/RightIndicators/BackIndicator").GetComponent<MeshRenderer>().material = yellow_light;
                    yield return new WaitForSeconds(0.5f);
                    go.transform.Find("Body/RightIndicators/FrontIndicator").GetComponent<MeshRenderer>().material = no_light;
                    go.transform.Find("Body/RightIndicators/SideIndicator").GetComponent<MeshRenderer>().material = no_light;
                    go.transform.Find("Body/RightIndicators/BackIndicator").GetComponent<MeshRenderer>().material = no_light;
                    yield return new WaitForSeconds(0.5f);
                }
                break;
        }
    }

    private IEnumerator RotateCar(GameObject go, string turn)
    {
        switch (turn)
        {
            case "left":
                if (fasterTurns.Contains(go.name))
                {
                    yield return new WaitForSeconds(1);
                    for (int i = 0; i < 10; i++)
                    {
                        go.transform.Rotate(Vector3.up, -4.5f);
                        yield return new WaitForSeconds(0.025f);
                    }
                }
                else
                {
                    yield return new WaitForSeconds(1);
                    for (int i = 0; i < 10; i++)
                    {
                        go.transform.Rotate(Vector3.up, -4.5f);
                        yield return new WaitForSeconds(0.05f);
                    }
                }
                break;
            case "right":
                yield return new WaitForSeconds(1);
                for (int i = 0; i < 10; i++)
                {
                    go.transform.Rotate(Vector3.up, 9);
                    yield return new WaitForSeconds(0.1f);
                }
                break;
        }
    }

    private IEnumerator MovePedestrian(GameObject obj,Vector3 newPosition,float speed)
    {
        Vector3 distance = (newPosition - obj.transform.position) / (10*speed);
        for (int i = 0; i < 10 * speed; i++)
        {
            if (obj != null)
            {
                obj.transform.position += distance;
                yield return new WaitForSeconds(0.1f);
            }
        }
    }
    private IEnumerator MoveCar(GameObject obj, Vector3 newPosition,float speed)
    {
        Vector3 distance = (newPosition - obj.transform.position) / (10 * speed);
        for (int i = 0; i < 10*speed; i++)
        {
            if (obj != null)
            {
                obj.transform.position += distance;
                yield return new WaitForSeconds(0.1f);
            }
        }
    }

    private void ChangeLights(string color)
    {
        GameObject [] goArray;
        switch (color)
        {
            case "red":
                goArray = GameObject.FindGameObjectsWithTag("SubRoadLight");
                foreach (GameObject light in goArray)
                {
                    StartCoroutine(ChangeOneLight(light, "green"));
                }
                goArray = GameObject.FindGameObjectsWithTag("PedestrianMainRoadLight");
                foreach (GameObject light in goArray)
                {
                    StartCoroutine(ChangeOnePedestrianLight(light, "green"));
                }
                break;
            case "green":
                goArray = GameObject.FindGameObjectsWithTag("MainRoadLight");
                foreach (GameObject light in goArray)
                {
                    StartCoroutine(ChangeOneLight(light, "green"));
                }
                goArray = GameObject.FindGameObjectsWithTag("PedestrianSubRoadLight");
                foreach (GameObject light in goArray)
                {
                    StartCoroutine(ChangeOnePedestrianLight(light, "green"));
                }
                break;
            case "yellow":
                goArray = GameObject.FindGameObjectsWithTag("MainRoadLight");
                foreach (GameObject light in goArray)
                {
                    StartCoroutine(ChangeOneLight(light, "yellow"));
                }
                
                goArray = GameObject.FindGameObjectsWithTag("SubRoadLight");
                foreach (GameObject light in goArray)
                {
                    StartCoroutine(ChangeOneLight(light, "yellow"));
                }
                
                goArray = GameObject.FindGameObjectsWithTag("PedestrianMainRoadLight");
                foreach (GameObject light in goArray)
                {
                    StartCoroutine(ChangeOnePedestrianLight(light, "yellow"));
                }
                goArray = GameObject.FindGameObjectsWithTag("PedestrianSubRoadLight");
                foreach (GameObject light in goArray)
                {
                    StartCoroutine(ChangeOnePedestrianLight(light, "yellow"));
                }
                
                break;
                
        }
    }

    private IEnumerator ChangeOneLight(GameObject light,String color)
    {
        switch (color)
        {
            case "green":
                light.transform.Find("Box/YellowLight").GetComponent<MeshRenderer>().material = yellow_light;
                yield return new WaitForSeconds(2);
                light.transform.Find("Box/GreenLight").GetComponent<MeshRenderer>().material = green_light;
                light.transform.Find("Box/YellowLight").GetComponent<MeshRenderer>().material = no_light;
                light.transform.Find("Box/RedLight").GetComponent<MeshRenderer>().material = no_light;
                break;
            case "yellow":
                if(light.transform.Find("Box/GreenLight").GetComponent<MeshRenderer>().sharedMaterial == green_light)
                {
                    light.transform.Find("Box/GreenLight").GetComponent<MeshRenderer>().material = no_light;
                    light.transform.Find("Box/YellowLight").GetComponent<MeshRenderer>().material = yellow_light;
                    yield return new WaitForSeconds(2);
                    light.transform.Find("Box/YellowLight").GetComponent<MeshRenderer>().material = no_light;
                    light.transform.Find("Box/RedLight").GetComponent<MeshRenderer>().material = red_light;
                }

                break;
        }
    }
    private IEnumerator ChangeOnePedestrianLight(GameObject light, String color)
    {
        switch (color)
        {
            case "yellow":
                for(int i = 0; i < 6; i++)
                {
                    light.transform.Find("Box/GreenLight").GetComponent<MeshRenderer>().material = no_light;
                    yield return new WaitForSeconds(0.25f);
                    light.transform.Find("Box/GreenLight").GetComponent<MeshRenderer>().material = green_light;
                    yield return new WaitForSeconds(0.25f);
                }
                light.transform.Find("Box/GreenLight").GetComponent<MeshRenderer>().material = no_light;
                light.transform.Find("Box/RedLight").GetComponent<MeshRenderer>().material = red_light;
                break;
            case "green":
                light.transform.Find("Box/RedLight").GetComponent<MeshRenderer>().material = no_light;
                light.transform.Find("Box/GreenLight").GetComponent<MeshRenderer>().material = green_light;
                break;
        }
        yield return null;
    }

    static void StartServer()
    {
        if (networkThread == null)
        {
            Connect();
            
            networkThread = new Thread(() =>
            {
                while (reader != null)
                {
                    Message msg = Message.ReadFromStream(reader);
                    AddItemToQueue(msg);
                }
                lock (networkThread)
                {
                    networkThread = null;
                }
            });
            networkThread.Start();
        }
        
    }

    static void Connect()
    {
        if(client == null)
        {
            string server = "localhost";
            int port = 12345;
            client = new TcpClient(server, port);
            Stream stream = client.GetStream();
            reader = new BinaryReader(stream);
            writer = new BinaryWriter(stream);
        }
    }

    public static void Send(Message msg)
    {
        msg.WriteToStream(writer);
        writer.Flush();
    }
}
