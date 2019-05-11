import React, { useState, useRef } from "react";
import Websocket from "react-websocket";
import { Switch, Route } from "react-router-dom";
import styled from "styled-components";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import { faCalendarPlus } from "@fortawesome/free-solid-svg-icons";

import { Button } from "../../styles/Buttons";

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
export function PublicWs() {
  const [enabled, setEnabled] = useState(false);
  const wsRef = useRef();
  const _input = useRef("");
  const _desc = useRef("");

  const handleChange = () => {
    if (!enabled) {
      _input.current.value.trim() && setEnabled(true);
    }

    return !_input.current.value.trim() && setEnabled(false);
  };

  const handleConnection = () => {
    console.log("connected to server");
  };

  const handleMessage = data => {
    console.log("message", data);
  };

  const createMeeting = e => {
    if (!enabled) {
      return;
    }
    e.preventDefault();
    wsRef.current.sendMessage(
      JSON.stringify({
        command: "create_meeting",
        data: {
          name: _input.current.value,
          agenda: [{ time: new Date().getTime(), title: _input.current.value }]
        }
      })
    );
  };

  const joinMeeting = () => {
    wsRef.current.sendMessage(
      JSON.stringify({
        command: "join",
        data: {
          id: "929292",
          group: "0202020"
        }
      })
    );
  };

  const sendMessage = () => {
    wsRef.current.sendMessage(
      JSON.stringify({
        command: "send",
        data: {
          id: "929292",
          group: "0202020",
          message: "I suggest Riesen"
        }
      })
    );
  };

  const nextEntry = () => {
    wsRef.current.sendMessage(JSON.stringify({ command: "next" }));
  };

  return (
    <>
      <Websocket
        url="ws://localhost:8080/websocket"
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
                    rows={4}
                  />
                  <Button enabled={enabled}>
                    <FontAwesomeIcon icon={faCalendarPlus} />
                  </Button>
                </Form>
              </div>
            )}
          />
          <Route
            path="/join"
            render={() => (
              <div>
                <button onClick={joinMeeting}>joinMeeting</button>
                <button onClick={sendMessage}>sendMessage</button>
                <button onClick={nextEntry}>nextEntry</button>
              </div>
            )}
          />
        </Switch>
      </FormContainer>
    </>
  );
}

export default PublicWs;
