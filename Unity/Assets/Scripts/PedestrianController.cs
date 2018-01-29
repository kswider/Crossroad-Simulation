using System.Collections;
using UnityEngine;

public class PedestrianController : MonoBehaviour
{
    public Vector3 OldPosition { get; set; }

    public void MovePedestrian(Vector3 newPosition,float speed)
    {
        StartCoroutine(MovePedestrianCoroutine(newPosition, speed));
    }

    private IEnumerator MovePedestrianCoroutine(Vector3 newPosition, float speed)
    {
        Vector3 distance = (newPosition - OldPosition) / (10 * speed);
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
}