import React, { useState, useRef } from "react";
import Websocket from "react-websocket";
import { useSelector, useDispatch } from "react-redux";
import { Switch, Route, withRouter } from "react-router-dom";
import styled from "styled-components";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import { faCalendarPlus, faSignInAlt } from "@fortawesome/free-solid-svg-icons";
import MeetingSpace from "../MeetingSpace";
import { ws } from "../../endpoints";
import { Button } from "../../styles/Buttons";
import { JOIN_MEETING } from "../../ducks/meetings";
import { Idea } from "../../styles/Icons";

const MeetingList = styled.div`
  display: flex;
  flex-wrap: wrap;
  justify-content: center;

  > * {
    box-shadow: 1px 2px 3px rgba(0, 0, 0, 0.3);
    padding: 0.25em;
    margin: 0.5em;
    display: flex;
    flex-direction: column;
    justify-content: center;
    align-items: center;
  }
`;

const FormContainer = styled.div`
  display: flex;
  flex-direction: column;
`;

const Form = styled.form`
  display: flex;
  flex-direction: column;

  > * {
    margin: 0.5em auto;

    &:focus {
      outline: none;
    }
  }

  > input,
  textarea {
    min-width: 300px;
    padding-left: 0.25em;
    border: none;
    border-bottom: 1px solid black;
    background: transparent;
    font-family: inherit;
    font-size: inherit;
  }

  > textarea {
    resize: none;
  }
`;

// this socket is to be used privately outside a meeting
export function PublicWs({ history }) {
  const [enabled, setEnabled] = useState(false);
  const wsRef = useRef();
  const _input = useRef("");
  const _desc = useRef("");
  const _points = useRef("");
  const _link = useRef("");
  const { id } = useSelector(({ auth }) => auth);
  const { meetings } = useSelector(({ meetings }) => meetings);
  const dispatch = useDispatch();

  const handleChange = () => {
    if (!enabled) {
      _input.current.value.trim() && setEnabled(true);
    }

    return !_input.current.value.trim() && setEnabled(false);
  };

  const handleConnection = () => {
    console.log("connected to server");
  };

  const handleMessage = (data = "{}") => {
    console.log("message", data);
    const parsed = JSON.parse(data);
    const { success, joined_meeting, ...rest } = parsed;
    if (success) {
      history.push("/");
    }
    if (joined_meeting) {
      dispatch({ type: JOIN_MEETING, ...rest });
      history.push("/meeting");
    }
  };

  const createMeeting = e => {
    if (!enabled) {
      return;
    }
    e.preventDefault();

    const now = new Date().getTime();
    const tenMinutes = 1000 * 60 * 10;

    const pointsInput = _points.current ? _points.current.value : "";
    const points = pointsInput.split("\n").map((point, index) => ({
      from: now + (index + 1) * tenMinutes,
      to: now + (index + 2) * tenMinutes,
      title: point
    }));

    wsRef.current.sendMessage(
      JSON.stringify({
        command: "create_meeting",
        data: {
          creator: id,
          name: _input.current.value.trim(),
          purpose: _desc.current.value,
          agenda: [
            {
              from: now,
              to: now + tenMinutes,
              title: _input.current.value.trim()
            },
            ...points
          ]
        }
      })
    );
  };

  const joinMeeting = group => () => {
    wsRef.current.sendMessage(
      JSON.stringify({
        command: "join",
        data: {
          id,
          group
        }
      })
    );
  };

  const sendMessage = (group, message) => {
    console.log(group, message, id);
    return wsRef.current.sendMessage(
      JSON.stringify({
        command: "send",
        data: {
          id,
          group,
          message
        }
      })
    );
  };

  const startMeeting = group => {
    wsRef.current.sendMessage(
      JSON.stringify({
        command: "start_meeting",
        data: {
          group
        }
      })
    );
  };

  const joinLink = e => {
    e.preventDefault();
    const link = _link.current.value;
    joinMeeting(link)();
  };

  return (
    <>
      <Websocket
        url={`${ws}/websocket`}
        onOpen={handleConnection}
        onMessage={handleMessage}
        ref={wsRef}
      />
      <FormContainer>
        <Switch>
          <Route
            path="/create"
            render={() => (
              <div>
                <Form onSubmit={createMeeting}>
                  <input
                    type="text"
                    ref={_input}
                    placeholder="Subject"
                    onChange={handleChange}
                  />
                  <textarea
                    type="text"
                    ref={_desc}
                    placeholder="Purpose"
                    rows={2}
                  />
                  <input type="text" placeholder="Location" />
                  <label htmlFor="agenda">Agenda</label>
                  <textarea
                    id="agenda"
                    type="text"
                    ref={_points}
                    placeholder="One topic per line."
                    rows={4}
                  />
                  <div>
                    <Idea />
                    <span>
                      Remember to plan enough time for each topic to cover
                      questions and discussions
                    </span>
                  </div>
                  <Button enabled={enabled}>
                    <FontAwesomeIcon icon={faCalendarPlus} />
                  </Button>
                </Form>

                <div />
              </div>
            )}
          />
          <Route
            path="/join"
            render={() => (
              <>
                <Form onSubmit={joinLink}>
                  <input type="text" ref={_link} placeholder="Got a link?" />
                  <button>Join</button>
                </Form>

                <MeetingList>
                  {meetings.map(meeting => (
                    <Button
                      key={meeting}
                      onClick={joinMeeting(meeting)}
                      enabled={true}
                    >
                      <span>Join {meeting.slice(0, 5)}</span>
                      <FontAwesomeIcon icon={faSignInAlt} />
                    </Button>
                  ))}
                </MeetingList>
              </>
            )}
          />
          <Route
            path="/meeting"
            render={() => (
              <MeetingSpace
                startMeeting={startMeeting}
                sendMessage={sendMessage}
              />
            )}
          />
        </Switch>
      </FormContainer>
    </>
  );
}

export default withRouter(PublicWs);
