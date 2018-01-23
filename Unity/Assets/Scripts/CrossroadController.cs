using Newtonsoft.Json.Linq;
using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Net.Sockets;
using System.Threading;
using UnityEngine;
using UnityEngine.UI;

public class CrossroadController : MonoBehaviour {

    [SerializeField]
    private GameObject pedestrian;
    [SerializeField]
    private GameObject car;
    [SerializeField]
    private Button but;
    [SerializeField]
    private Material red_light;
    [SerializeField]
    private Material yellow_light;
    [SerializeField]
    private Material green_light;
    [SerializeField]
    private Material no_light;
    static TcpClient client = null;
    static BinaryReader reader = null;
    static BinaryWriter writer = null;
    static Thread networkThread = null;
    private static Queue<Message> messageQueue = new Queue<Message>();

    void Awake()
    {
        DontDestroyOnLoad(this);
    }

	// Use this for initialization
	void Start () {
        StartServer();
        but.onClick.AddListener(sendmsg);

    }

    private void sendmsg()
    {
        Send(new Message(new Byte[] { 1, 2 }));
    }

    // Update is called once per frame
    void Update () {
        processMessage();

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
            Debug.Log(myJson);
            JObject json = JObject.Parse(myJson);

            String action = json["action"].ToString();
            String pid;
            //float x, z;
            GameObject go;
            switch (action)
            {
                
                case "pedestrian_spawned":
                    pid = json["pid"].ToString();
                    go = GameObject.Instantiate(pedestrian);
                    go.name = pid;
                    float x = float.Parse(json["position_x"].ToString()) * 2;
                    float z = float.Parse(json["position_y"].ToString()) * 2;
                    go.transform.position.Set(x, 0.375f, z);
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
                    go.transform.position.Set(x, 0.5f, z);
                    break;
                case "car_spawned":
                    pid = json["pid"].ToString();
                    go = GameObject.Instantiate(car);
                    go.name = pid;
                    x = float.Parse(json["position_x"].ToString()) * 2;
                    z = float.Parse(json["position_y"].ToString()) * 2;
                    go.transform.position.Set(x, 0, z);
                    break;
                case "car_disappeared":
                    pid = json["pid"].ToString();
                    go = GameObject.Find(pid);
                    GameObject.Destroy(go);
                    break;
                case "car_move":
                    pid = json["pid"].ToString();
                    go = GameObject.Find(pid);
                    x = float.Parse(json["position_x"].ToString()) * 2;
                    z = float.Parse(json["position_y"].ToString()) * 2;
                    go.transform.position.Set(x, 0, z);
                    break;
                case "lights_changes_to_red":
                    ChangeLights("red");
                    break;
                case "lights_changes_to_green":
                    ChangeLights("green");
                    break;
            }
        }
    }

    private void ChangeLights(string color)
    {
        GameObject [] goArray;
        switch (color)
        {
            case "red":
                goArray = GameObject.FindGameObjectsWithTag("MainRoadLight");
                foreach (GameObject light in goArray)
                {
                    StartCoroutine(ChangeOneLight(light, "red"));
                }
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
                goArray = GameObject.FindGameObjectsWithTag("PedestrianSubRoadLight");
                foreach (GameObject light in goArray)
                {
                    StartCoroutine(ChangeOnePedestrianLight(light, "red"));
                }
                break;
            case "green":
                goArray = GameObject.FindGameObjectsWithTag("MainRoadLight");
                foreach (GameObject light in goArray)
                {
                    StartCoroutine(ChangeOneLight(light, "green"));
                }
                goArray = GameObject.FindGameObjectsWithTag("SubRoadLight");
                foreach (GameObject light in goArray)
                {
                    StartCoroutine(ChangeOneLight(light, "red"));
                }
                goArray = GameObject.FindGameObjectsWithTag("PedestrianMainRoadLight");
                foreach (GameObject light in goArray)
                {
                    StartCoroutine(ChangeOnePedestrianLight(light, "red"));
                }
                goArray = GameObject.FindGameObjectsWithTag("PedestrianSubRoadLight");
                foreach (GameObject light in goArray)
                {
                    StartCoroutine(ChangeOnePedestrianLight(light, "green"));
                }
                break;
        }
    }

    private IEnumerator ChangeOneLight(GameObject light,String color)
    {
        switch (color)
        {
            case "red":
                light.transform.Find("Box/GreenLight").GetComponent<MeshRenderer>().material = no_light;
                light.transform.Find("Box/YellowLight").GetComponent<MeshRenderer>().material = yellow_light;
                yield return new WaitForSeconds(2);
                light.transform.Find("Box/YellowLight").GetComponent<MeshRenderer>().material = no_light;
                light.transform.Find("Box/RedLight").GetComponent<MeshRenderer>().material = red_light;
                break;
            case "green":
                light.transform.Find("Box/YellowLight").GetComponent<MeshRenderer>().material = yellow_light;
                yield return new WaitForSeconds(2);
                light.transform.Find("Box/GreenLight").GetComponent<MeshRenderer>().material = green_light;
                light.transform.Find("Box/YellowLight").GetComponent<MeshRenderer>().material = no_light;
                light.transform.Find("Box/RedLight").GetComponent<MeshRenderer>().material = no_light;
                break;
        }
    }
    private IEnumerator ChangeOnePedestrianLight(GameObject light, String color)
    {
        switch (color)
        {
            case "red":
                for(int i = 0; i < 4; i++)
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

    IEnumerator ExecuteAfterTime(float seconds)
    {
        yield return new WaitForSeconds(seconds);
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
                    Debug.Log("reading from socket");
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
