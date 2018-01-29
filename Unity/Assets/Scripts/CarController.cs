using System.Collections;
using UnityEngine;

public class CarController : MonoBehaviour
{
    //Materials
    [SerializeField]
    private Material no_light;
    [SerializeField]
    private Material yellow_light;

    public Vector3 OldPosition { get; set; }

    public void StartIndicator(string turn)
    {
        StartCoroutine(IndicatorCoroutine(turn));
    }

    public void MoveCar(Vector3 newPosition,float speed)
    {
        StartCoroutine(MoveCarCoroutine(newPosition, speed));
    }

    public void RotateCar(string turn,bool faster = false)
    {
        switch (turn)
        {
            case "left":
                if (faster)
                    StartCoroutine(RotateCarLeftFasterCoroutine());
                else
                    StartCoroutine(RotateCarLeftCoroutine());
                break;
            case "right":
                StartCoroutine(RotateCarRightCoroutine());
                break;
        }
    }

    private IEnumerator MoveCarCoroutine(Vector3 newPosition, float speed)
    {
        Vector3 distance = ((newPosition - OldPosition) / (10 * speed));
        OldPosition = newPosition;
        for (int i = 0; i < 10 * speed; i++)
        {
            if (this != null)
            {
                transform.position += distance;
                yield return new WaitForSeconds(0.1f);
            }
        }
    }

    private IEnumerator IndicatorCoroutine(string which)
    {
        switch (which)
        {
            case "left":
                while (this != null)
                {
                    transform.Find("Body/LeftIndicators/FrontIndicator").GetComponent<MeshRenderer>().material = yellow_light;
                    transform.Find("Body/LeftIndicators/SideIndicator").GetComponent<MeshRenderer>().material = yellow_light;
                    transform.Find("Body/LeftIndicators/BackIndicator").GetComponent<MeshRenderer>().material = yellow_light;
                    yield return new WaitForSeconds(0.33f);
                    transform.Find("Body/LeftIndicators/FrontIndicator").GetComponent<MeshRenderer>().material = no_light;
                    transform.Find("Body/LeftIndicators/SideIndicator").GetComponent<MeshRenderer>().material = no_light;
                    transform.Find("Body/LeftIndicators/BackIndicator").GetComponent<MeshRenderer>().material = no_light;
                    yield return new WaitForSeconds(0.33f);
                }
                break;
            case "right":
                while (this != null)
                {
                    transform.Find("Body/RightIndicators/FrontIndicator").GetComponent<MeshRenderer>().material = yellow_light;
                    transform.Find("Body/RightIndicators/SideIndicator").GetComponent<MeshRenderer>().material = yellow_light;
                    transform.Find("Body/RightIndicators/BackIndicator").GetComponent<MeshRenderer>().material = yellow_light;
                    yield return new WaitForSeconds(0.5f);
                    transform.Find("Body/RightIndicators/FrontIndicator").GetComponent<MeshRenderer>().material = no_light;
                    transform.Find("Body/RightIndicators/SideIndicator").GetComponent<MeshRenderer>().material = no_light;
                    transform.Find("Body/RightIndicators/BackIndicator").GetComponent<MeshRenderer>().material = no_light;
                    yield return new WaitForSeconds(0.5f);
                }
                break;
        }
    }

    private IEnumerator RotateCarLeftFasterCoroutine()
    {
        for (int i = 0; i < 10; i++)
        {
            transform.Rotate(Vector3.up, -4.5f);
            yield return new WaitForSeconds(0.025f);
        }
    }
    private IEnumerator RotateCarLeftCoroutine()
    {
        yield return new WaitForSeconds(1);
        for (int i = 0; i < 10; i++)
        {
            transform.Rotate(Vector3.up, -4.5f);
            yield return new WaitForSeconds(0.05f);
        }
    }
    private IEnumerator RotateCarRightCoroutine()
    {
        yield return new WaitForSeconds(1);
        for (int i = 0; i < 10; i++)
        {
            transform.Rotate(Vector3.up, 9);
            yield return new WaitForSeconds(0.1f);
        }       
    }
}